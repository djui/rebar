%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009, 2010 Dave Smith (dizzyd@dizzyd.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
%% @author Dave Smith <dizzyd@dizzyd.com>
%% @doc rebar_eunit supports the following commands:
%% <ul>
%%   <li>eunit - runs eunit tests</li>
%%   <li>clean - remove .eunit directory</li>
%% </ul>
%% The following Global options are supported:
%% <ul>
%%   <li>verbose=1 - show extra output from the eunit test</li>
%%   <li>suite="foo"" - runs test/foo_tests.erl</li>
%% </ul>
%% Additionally, for projects that have separate folders for the core
%% implementation, and for the unit tests, then the following
%% <code>rebar.config</code> option can be provided:
%% <code>{eunit_compile_opts, [{src_dirs, ["dir"]}]}.</code>.
%% @copyright 2009, 2010 Dave Smith
%% -------------------------------------------------------------------
-module(rebar_eunit).

-export([eunit/2,
reconstruct_app_env_vars/1,
         clean/2]).

-include("rebar.hrl").

-define(EUNIT_DIR, ".eunit").

%% ===================================================================
%% Public API
%% ===================================================================

eunit(Config, AppFile) ->
    %% Check for app global parameter; this is a comma-delimited list
    %% of apps on which we want to run eunit
    case rebar_config:get_global(app, undefined) of
        undefined ->
            %% No app parameter specified, check the skip list..
            case rebar_config:get_global(skip_app, undefined) of
                undefined ->
                    %% no skip list, run everything..
                    ok;
                SkipApps ->
                    TargetApps = [list_to_atom(A) ||
                                     A <- string:tokens(SkipApps, ",")],
                    ThisApp = rebar_app_utils:app_name(AppFile),
                    case lists:member(ThisApp, TargetApps) of
                        false ->
                            ok;
                        true ->
                            ?DEBUG("Skipping eunit on app: ~p\n", [ThisApp]),
                            throw(ok)
                    end
            end;
        Apps ->
            TargetApps = [list_to_atom(A) || A <- string:tokens(Apps, ",")],
            ThisApp = rebar_app_utils:app_name(AppFile),
            case lists:member(ThisApp, TargetApps) of
                true ->
                    ok;
                false ->
                    ?DEBUG("Skipping eunit on app: ~p\n", [ThisApp]),
                    throw(ok)
            end
    end,

    %% Make sure ?EUNIT_DIR/ and ebin/ directory exists (tack on dummy module)
    ok = filelib:ensure_dir(eunit_dir() ++ "/foo"),
    ok = filelib:ensure_dir(ebin_dir() ++ "/foo"),

    %% Setup code path prior to compilation so that parse_transforms
    %% and the like work properly. Also, be sure to add ebin_dir()
    %% to the END of the code path so that we don't have to jump
    %% through hoops to access the .app file
    CodePath = code:get_path(),
    true = code:add_patha(eunit_dir()),
    true = code:add_pathz(ebin_dir()),

    %% Obtain all the test modules for inclusion in the compile stage.
    %% Notice: this could also be achieved with the following
    %% rebar.config option: {eunit_compile_opts, [{src_dirs, ["test"]}]}
    TestErls = rebar_utils:find_files("test", ".*\\.erl\$"),

    %% Copy source files to eunit dir for cover in case they are not directly
    %% in src but in a subdirectory of src. Cover only looks in cwd and ../src
    %% for source files.
    SrcErls = rebar_utils:find_files("src", ".*\\.erl\$"),
    ok = rebar_file_utils:cp_r(SrcErls ++ TestErls, ?EUNIT_DIR),

    %% Compile erlang code to ?EUNIT_DIR, using a tweaked config
    %% with appropriate defines for eunit, and include all the test modules
    %% as well.
    rebar_erlc_compiler:doterl_compile(eunit_config(Config),
                                       ?EUNIT_DIR, TestErls),

    %% Build a list of all the .beams in ?EUNIT_DIR -- use this for cover
    %% and eunit testing. Normally you can just tell cover and/or eunit to
    %% scan the directory for you, but eunit does a code:purge in conjunction
    %% with that scan and causes any cover compilation info to be lost.
    %% Filter out "*_tests" modules so eunit won't doubly run them and
    %% so cover only calculates coverage on production code.
    BeamFiles = [N || N <- rebar_utils:beams(?EUNIT_DIR),
                      string:str(N, "_tests.beam") =:= 0],
    Modules = [rebar_utils:beam_to_mod(?EUNIT_DIR, N) || N <- BeamFiles],
    SrcModules = [rebar_utils:erl_to_mod(M) || M <- SrcErls],

    StatusBefore = status_before_eunit(),

    cover_init(Config, BeamFiles),
    EunitResult = perform_eunit(Config, Modules),
    perform_cover(Config, Modules, SrcModules),

    cleanup_after_eunit(StatusBefore),

    case EunitResult of
        ok ->
            ok;
        _ ->
            ?ABORT("One or more eunit tests failed.~n", [])
    end,

    %% Restore code path
    true = code:set_path(CodePath),
    ok.

clean(_Config, _File) ->
    rebar_file_utils:rm_rf(?EUNIT_DIR).

%% ===================================================================
%% Internal functions
%% ===================================================================

eunit_dir() ->
    filename:join(rebar_utils:get_cwd(), ?EUNIT_DIR).

ebin_dir() ->
    filename:join(rebar_utils:get_cwd(), "ebin").

perform_eunit(Config, Modules) ->
io:format(user, "Config ~p\n", [Config]),
io:format(user, "Modules ~p\n", [Modules]),
    %% suite defined, so only specify the module that relates to the
    %% suite (if any). Suite can be a comma seperated list of modules to run.
    Suite = rebar_config:get_global(suite, undefined),
    EunitOpts = get_eunit_opts(Config),

    %% Move down into ?EUNIT_DIR while we run tests so any generated files
    %% are created there (versus in the source dir)
    Cwd = rebar_utils:get_cwd(),
    ok = file:set_cwd(?EUNIT_DIR),

    EunitResult = perform_eunit(EunitOpts, Modules, Suite),

    %% Return to original working dir
    ok = file:set_cwd(Cwd),

    EunitResult.

perform_eunit(EunitOpts, Modules, undefined) ->
    (catch eunit:test(Modules, EunitOpts));
perform_eunit(EunitOpts, _Modules, Suites) ->
    (catch eunit:test([list_to_atom(Suite) ||
                          Suite <- string:tokens(Suites, ",")], EunitOpts)).

get_eunit_opts(Config) ->
    %% Enable verbose in eunit if so requested..
    BaseOpts = case rebar_config:is_verbose() of
                   true ->
                       [verbose];
                   false ->
                       []
               end,

    BaseOpts ++ rebar_config:get_list(Config, eunit_opts, []).

eunit_config(Config) ->
    EqcOpts = eqc_opts(),
    PropErOpts = proper_opts(),

    ErlOpts = rebar_config:get_list(Config, erl_opts, []),
    EunitOpts = rebar_config:get_list(Config, eunit_compile_opts, []),
    Opts0 = [{d, 'TEST'}] ++
        ErlOpts ++ EunitOpts ++ EqcOpts ++ PropErOpts,
    Opts = [O || O <- Opts0, O =/= no_debug_info],
    Config1 = rebar_config:set(Config, erl_opts, Opts),

    FirstErls = rebar_config:get_list(Config1, eunit_first_files, []),
    rebar_config:set(Config1, erl_first_files, FirstErls).

eqc_opts() ->
    define_if('EQC', is_lib_avail(is_eqc_avail, eqc,
                                  "eqc.hrl", "QuickCheck")).

proper_opts() ->
    define_if('PROPER', is_lib_avail(is_proper_avail, proper,
                                     "proper.hrl", "PropEr")).

define_if(Def, true) -> [{d, Def}];
define_if(_Def, false) -> [].

is_lib_avail(DictKey, Mod, Hrl, Name) ->
    case erlang:get(DictKey) of
        undefined ->
            IsAvail = case code:lib_dir(Mod, include) of
                          {error, bad_name} ->
                              false;
                          Dir ->
                              filelib:is_regular(filename:join(Dir, Hrl))
                      end,
            erlang:put(DictKey, IsAvail),
            ?DEBUG("~s availability: ~p\n", [Name, IsAvail]),
            IsAvail;
        IsAvail ->
            IsAvail
    end.

perform_cover(Config, BeamFiles, SrcModules) ->
    perform_cover(rebar_config:get(Config, cover_enabled, false),
                  Config, BeamFiles, SrcModules).

perform_cover(false, _Config, _BeamFiles, _SrcModules) ->
    ok;
perform_cover(true, Config, BeamFiles, SrcModules) ->
    cover_analyze(Config, BeamFiles, SrcModules).

cover_analyze(_Config, [], _SrcModules) ->
    ok;
cover_analyze(Config, Modules, SrcModules) ->
    %% suite can be a comma seperated list of modules to test
    Suite = [list_to_atom(S) ||
                S <- string:tokens(rebar_config:get_global(suite, ""), ",")],
    FilteredModules = case Suite of
                          [] -> Modules;
                          _  -> [M || M <- Modules, lists:member(M, Suite)]
                      end,

    %% Generate coverage info for all the cover-compiled modules
    Coverage = [cover_analyze_mod(M) || M <- FilteredModules],

    %% Write index of coverage info
    cover_write_index(lists:sort(Coverage), SrcModules),

    %% Write coverage details for each file
    lists:foreach(fun({M, _, _}) ->
                          {ok, _} = cover:analyze_to_file(M, cover_file(M),
                                                          [html])
                  end, Coverage),

    Index = filename:join([rebar_utils:get_cwd(), ?EUNIT_DIR, "index.html"]),
    ?CONSOLE("Cover analysis: ~s\n", [Index]),

    %% Print coverage report, if configured
    case rebar_config:get(Config, cover_print_enabled, false) of
        true ->
            cover_print_coverage(lists:sort(Coverage));
        false ->
            ok
    end.

cover_init(false, _BeamFiles) ->
    ok;
cover_init(true, BeamFiles) ->
    %% Make sure any previous runs of cover don't unduly influence
    cover:reset(),

    ?INFO("Cover compiling ~s\n", [rebar_utils:get_cwd()]),

    Compiled = [{Beam, cover:compile_beam(Beam)} || Beam <- BeamFiles],
    case [Module || {_, {ok, Module}} <- Compiled] of
        [] ->
            %% No modules compiled successfully...fail
            ?ERROR("Cover failed to compile any modules; aborting.~n", []),
            ?FAIL;
        _ ->
            %% At least one module compiled successfully

            %% It's not an error for cover compilation to fail partially,
            %% but we do want to warn about them
            PrintWarning =
                fun(Beam, Desc) ->
                        ?CONSOLE("Cover compilation warning for ~p: ~p",
                                 [Beam, Desc])
                end,
            _ = [PrintWarning(Beam, Desc) || {Beam, {error, Desc}} <- Compiled],
            ok
    end;
cover_init(Config, BeamFiles) ->
    cover_init(rebar_config:get(Config, cover_enabled, false), BeamFiles).

cover_analyze_mod(Module) ->
    case cover:analyze(Module, coverage, module) of
        {ok, {Module, {Covered, NotCovered}}} ->
            %% Modules that include the eunit header get an implicit
            %% test/0 fun, which cover considers a runnable line, but
            %% eunit:test(TestRepresentation) never calls.  Decrement
            %% NotCovered in this case.
            align_notcovered_count(Module, Covered, NotCovered,
                                   is_eunitized(Module));
        {error, Reason} ->
            ?ERROR("Cover analyze failed for ~p: ~p ~p\n",
                   [Module, Reason, code:which(Module)]),
            {0,0}
    end.

is_eunitized(Mod) ->
    has_eunit_test_fun(Mod) andalso
        has_header(Mod, "include/eunit.hrl").

has_eunit_test_fun(Mod) ->
    [F || {exports, Funs} <- Mod:module_info(),
          {F, 0} <- Funs, F =:= test] =/= [].

has_header(Mod, Header) ->
    Mod1 = case code:which(Mod) of
               cover_compiled ->
                   {file, File} = cover:is_compiled(Mod),
                   File;
               non_existing -> Mod;
               preloaded -> Mod;
               L -> L
           end,
    {ok, {_, [{abstract_code, {_, AC}}]}} = beam_lib:chunks(Mod1,
                                                            [abstract_code]),
    [F || {attribute, 1, file, {F, 1}} <- AC,
          string:str(F, Header) =/= 0] =/= [].

align_notcovered_count(Module, Covered, NotCovered, false) ->
    {Module, Covered, NotCovered};
align_notcovered_count(Module, Covered, NotCovered, true) ->
    {Module, Covered, NotCovered - 1}.

cover_write_index(Coverage, SrcModules) ->
    {ok, F} = file:open(filename:join([?EUNIT_DIR, "index.html"]), [write]),
    ok = file:write(F, "<html><head><title>Coverage Summary</title></head>\n"),
    IsSrcCoverage = fun({Mod,_C,_N}) -> lists:member(Mod, SrcModules) end,
    {SrcCoverage, TestCoverage} = lists:partition(IsSrcCoverage, Coverage),
    cover_write_index_section(F, "Source", SrcCoverage),
    cover_write_index_section(F, "Test", TestCoverage),
    ok = file:write(F, "</body></html>"),
    ok = file:close(F).

cover_write_index_section(_F, _SectionName, []) ->
    ok;
cover_write_index_section(F, SectionName, Coverage) ->
    %% Calculate total coverage %
    {Covered, NotCovered} = lists:foldl(fun({_Mod, C, N}, {CAcc, NAcc}) ->
                                                {CAcc + C, NAcc + N}
                                        end, {0, 0}, Coverage),
    TotalCoverage = percentage(Covered, NotCovered),

    %% Write the report
    ok = file:write(F, ?FMT("<body><h1>~s Summary</h1>\n", [SectionName])),
    ok = file:write(F, ?FMT("<h3>Total: ~s</h3>\n", [TotalCoverage])),
    ok = file:write(F, "<table><tr><th>Module</th><th>Coverage %</th></tr>\n"),

    FmtLink =
        fun(Module, Cov, NotCov) ->
                ?FMT("<tr><td><a href='~s.COVER.html'>~s</a></td><td>~s</td>\n",
                     [Module, Module, percentage(Cov, NotCov)])
        end,
    lists:foreach(fun({Module, Cov, NotCov}) ->
                          ok = file:write(F, FmtLink(Module, Cov, NotCov))
                  end, Coverage),
    ok = file:write(F, "</table>\n").

cover_print_coverage(Coverage) ->
    {Covered, NotCovered} = lists:foldl(fun({_Mod, C, N}, {CAcc, NAcc}) ->
                                                {CAcc + C, NAcc + N}
                                        end, {0, 0}, Coverage),
    TotalCoverage = percentage(Covered, NotCovered),

    %% Determine the longest module name for right-padding
    Width = lists:foldl(fun({Mod, _, _}, Acc) ->
                                case length(atom_to_list(Mod)) of
                                    N when N > Acc ->
                                        N;
                                    _ ->
                                        Acc
                                end
                        end, 0, Coverage) * -1,

    %% Print the output the console
    ?CONSOLE("~nCode Coverage:~n", []),
    lists:foreach(fun({Mod, C, N}) ->
                          ?CONSOLE("~*s : ~3s~n",
                                   [Width, Mod, percentage(C, N)])
                  end, Coverage),
    ?CONSOLE("~n~*s : ~s~n", [Width, "Total", TotalCoverage]).

cover_file(Module) ->
    filename:join([?EUNIT_DIR, atom_to_list(Module) ++ ".COVER.html"]).

percentage(0, 0) ->
    "not executed";
percentage(Cov, NotCov) ->
    integer_to_list(trunc((Cov / (Cov + NotCov)) * 100)) ++ "%".

get_app_names() ->
    [AppName || {AppName, _, _} <- application:loaded_applications()].

status_before_eunit() ->
    Apps = get_app_names(),
    AppEnvs = [{App, application:get_all_env(App)} || App <- Apps],
    %% DELME!
    %% AppEnvs = [{App, case application:get_all_env(App) of
    %%                      undefined -> [];
    %%                      Else      -> Else
    %%                  end} || App <- Apps],
    {erlang:processes(), erlang:is_alive(), AppEnvs, ets:tab2list(ac_tab)}.

cleanup_after_eunit({OldProcesses, WasAlive, OldAppEnvs, _OldACs}) ->
    IsAlive = erlang:is_alive(),
    if not WasAlive andalso IsAlive ->
            io:format(user, "KILL stopping net kernel....\n", []),
            erl_epmd:stop(),
            net_kernel:stop(),
            timer:sleep(100);
       true ->
            ok
    end,

    Processes = erlang:processes(),
    kill_extras(Processes -- OldProcesses),

    OldApps = [App || {App, _} <- OldAppEnvs],
    Apps = get_app_names(),
    [begin
         case lists:member(App, OldApps) of
             true  -> ok;
             false -> application:stop(App)
         end,
         application:unset_env(App, K)
     end || App <- Apps, App /= rebar,
            {K, _V} <- application:get_all_env(App)],

    reconstruct_app_env_vars(Apps),

    %% %% OK, this works, except that it can't help for apps like sasl......
    %% [begin
    %%      OldKVs = proplists:get_value(App, OldAppEnvs, []),
    %%      io:format(user, "XXX: app ~p OldKVs ~p\n", [App, OldKVs]),
    %%      [application:set_env(App, K, V) || {K, V} <- OldKVs]
    %%  end || App <- Apps],

    %% DELME this is broken??? Screws up rebar_log:log/3???
    %% case lists:member(sasl, Apps) of
    %%     true ->
    %%         case application:get_env(sasl, sasl_error_logger) of
    %%             undefined ->
    %%                 io:format(user, "XXX: reset tty\n", []),
    %%                 application:setenv(sasl, sasl_error_logger, tty);
    %%             _ ->
    %%                 ok
    %%         end;
    %%     _ ->
    %%         ok
    %% end,

    %% !@#$!@##$!@#$!#$%@#%$#!!@#$%!@#$% This doesn't work either:
    %% Uncaught error in rebar_core: {'EXIT',{{badmatch,undefined},
    %%                                       [{rebar_log,log,3},
    %%                                        {rebar_core,process_dir,4},....
    %% _ = application:unload(sasl),
    %% _ = application:load(sasl),

    %% %% Humbug ... this is what I get for not using slave to run the
    %% %% eunit tests.  You cannot use application:load/1 to reset the
    %% %% OTP application environment variables for an app unless the
    %% %% application is first unload/1'ed ... but R14B03 (and perhaps
    %% %% others) can arbitrarily kill the kernel application by
    %% %% unloading some applications ... even when unloading an app not
    %% %% included in Ericsson's Erlang/OTP distribution, such as
    %% %% riak_core, !@#$!.

    %% %% _ = application:which_applications(),
    %% %% timer:sleep(1000),
    %% %% _ = application:which_applications(),
    %% [ets:delete(ac_tab, K) || {K, _V} <- OldACs],
    %% [ets:insert(ac_tab, KV) || KV <- OldACs],
    %% true = (lists:sort(OldACs) == lists:sort(ets:tab2list(ac_tab))),

    %% %% BUGGY, delme?
    %% %% [begin
    %% %%      if App == kernel orelse App == stdlib ->
    %% %%              io:format(user, "XXX: skip app ~p\n", [App]),
    %% %%              ok;
    %% %%         true ->
    %% %%              io:format(user, "XXX: unload app ~p\n", [App]),
    %% %%              application:unload(App)
    %% %%      end,
    %% %%      application:load(App)
    %% %%  end || {App, _KVs} <- OldAppEnvs],
    %% [begin
    %%      OldKVs = proplists:get_value(App, OldAppEnvs, []),
    %%      io:format(user, "app ~p OldKVs ~p\n", [App, OldKVs]),
    %%      [application:set_env(App, K, V) || {K, V} <- OldKVs]
    %%  end || App <- Apps],
    %% [begin
    %%      io:format(user, "XXX: Unloading ~p\n", [App]),
    %%      catch application:unload(App)
    %%  end || App <- Apps, not lists:member(
    %%                            App, OldApps ++ [crypto, public_key, ssl])],

    ok.

kill_extras(Pids) ->
    KeepProcs = [cover_server, eunit_server, inet_gethost_native_sup,
                 inet_gethost_native, timer_server,
                 os_cmd_port_creator_MAYBE],
    Killed = [begin
                  Info = case erlang:process_info(Pid) of
                             undefined -> [];
                             Else      -> Else
                         end,
                  Keep = case proplists:get_value(registered_name, Info) of
                             undefined ->
                                 false;
                             Name ->
                                 lists:member(Name, KeepProcs)
                         end,
                  if Keep ->
                          ok;
                     true ->
                          io:format(user, "KILL ~p ~p\n", [Pid, Info]),
                          exit(Pid, kill),
                          Pid
                  end
              end || Pid <- Pids],
    case lists:usort(Killed) -- [ok] of
        [] ->
            io:format(user, "Nothing to KILL\n", []),
            [];
        Else ->
            timer:sleep(10),                    % Let deaths really happen...
            Else
    end.

reconstruct_app_env_vars([App|Apps]) ->
    CmdLine0 = proplists:get_value(App, init:get_arguments(), []),
    CmdVars = [{list_to_atom(K), list_to_atom(V)} || {K, V} <- CmdLine0],
    AppFile = code:lib_dir(App) ++ "/ebin/" ++ atom_to_list(App) ++ ".app",
    AppVars = case file:consult(AppFile) of
                  {ok, [{application, App, Ps}]} ->
                      proplists:get_value(env, Ps, []);
                  _ ->
                      []
              end,
    AllVars = CmdVars ++ AppVars,
    io:format(user, "XXX: reconstruct ~p ~p\n", [App, AllVars]),
    [application:set_env(App, K, V) || {K, V} <- AllVars],
    reconstruct_app_env_vars(Apps);
reconstruct_app_env_vars([]) ->
    ok.

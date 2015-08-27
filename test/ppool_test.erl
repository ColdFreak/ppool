-module(ppool_tests).
-include_lib("eunit/include/eunit.hrl").
-export([test_mfa/1, wait_mfa/1]).

%%% All Test Fixtures
start_test_() ->
    {"It should be possible to start a pool server and give it a name",
      {setup,
       fun find_unique_name/0,
       fun(Name) -> [start_and_test_name(Name)] end}}.

mfa_test_() ->
    {"A pool process can be allocated which will be ordered "
     "to run an MFA call determined at start time, with arguments "
     "provided at call time",
     {setup,
      fun start_pool/0,
      fun kill_ppool/1,
      fun(Name) -> [pool_run_mfa(Name)] end}}.

alloc_test_() ->
    {"A pool process can be allocated which will be ordered "
     "to run a worker, only if there are enough which "
     "haven't been ordered to run yet.",
     {set, 
      fun start_pool/0,
      fun kill_ppool/1,
      fun(Name) -> [pool_run_alloc(Name), pool_run_noalloc(Name) end].




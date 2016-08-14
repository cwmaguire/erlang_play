# erlang_play

## Various tests and experiments in Erlang

- deep/deep/deep/deep/deep/deep/ebin
  - -pa foo/*/ebin only goes one level deep
- build stuff
  - Makefile
  - erlang.mk
- access_non_exported_fully_qualified_fun.erl
- ansi_escape.erl
  - Screwing around with terminal effects
  - [ansi escape codes](https://en.wikipedia.org/wiki/ANSI_escape_code)
- benford.erl
  - Playing around with Benford's Law
- case_shadow.erl
  - Testing case statement variable shadowing
  - [Erlang examples](http://erlang.org/doc/programming_examples/list_comprehensions.html)
- chem_sig.erl
  - modelling simple chemical signalling
- consult
  - Not sure where I was using this
- deep
  - I think was using this to check how import_lib worked
- def_undefined.erl
  - Playing around with records
- echo_hex
  - escript to trying a fix goofy utf8 in the shell
- empty
  - Not sure where I was using this
- escript stuff
  - es: Show escript args
  - escript_path
  - get_env_var: read environment variables
- fsm_app
  - I think I build an FSM app to test out sys:get_status
- get_msgs.erl
  - using process_info
- include
  - testing putting record access functions in the .hrl file along with the record
- macro_test.erl
  - A bunch of experiments with macros
- make_users
  - Building a CSV of user data?
- msgs.erl
  - Some kind of small experiment with messages
- make_users.erl
  - Building a CSV of user data? (.erl version?)
- mines.erl
  - Find the optimal square to click in Minesweeper
- mult_behaviours
  - Checking if I can implement multiple behaviours
- png
  - Building a PNG decoder
  - Moved to its [own project](https://github.com/cwmaguire/erl_png)
  - srgb.erl
    - I dunno know, something to do with researching the sRGB colour space
- specs
  - missing_spec_type.erl
    - Trying to figure out spec types when upgrading from R16B02 to 18.2
  - tuple_type.erl
  - tuple_type.ex
  - type_error.erl
  - types.erl
- supervisor_and_gen_server
  - What happens when you supervisor:terminate_child on a gen_server with messages in its mailbox
- uninstall_meck.erl
  - Confirming how uninstall_meck works

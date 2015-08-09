all:
	pulp build --to dist/all.js

build:
	pulp -w build

tests:
	pulp -w test

hello:
	./bin/build_main.sh all_hello.js Daimyo.Hello helloMain

ui_thermite_factorial:
	./bin/build_main.sh all_ui_thermite_factorial.js Daimyo.UI.Thermite.Factorial uiThermiteFactorialMain

ui_thermite_ping:
	./bin/build_main.sh all_ui_thermite_ping.js Daimyo.UI.Thermite.Ping uiPingMain

ui_halogen_ping:
	./bin/build_main.sh all_ui_halogen_ping.js Daimyo.UI.Halogen.Ping uiHalogenPingMain

ui_halogen_todo_simple:
	./bin/build_main.sh all_ui_halogen_todo_simple.js Daimyo.UI.Halogen.Todo.Simple uiHalogenTodoSimpleMain

ui_halogen_list:
	./bin/build_main.sh all_ui_halogen_list.js Daimyo.UI.Halogen.List uiHalogenListMain

ui_halogen_set:
	./bin/build_main.sh all_ui_halogen_set.js Daimyo.UI.Halogen.Set uiHalogenSetMain

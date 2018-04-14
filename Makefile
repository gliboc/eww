all:
	erl -make

shell: all
	erl -pa ebin/

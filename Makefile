.PHONY: prepare test lint clean

prepare:
	eldev --unstable prepare

test:
	eldev --unstable test

lint:
	eldev --unstable lint

clean:
	eldev --unstable clean

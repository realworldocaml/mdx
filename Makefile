.PHONY: all test

all:
	jbuilder build --dev

test:
	jbuilder runtest --dev --force

clean:
	jbuilder clean

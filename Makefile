SBCL = sbcl

all: check

check:
	$(SBCL) --noinform --noprint \
	     --eval "(format t \"Running CAMBL unit tests...~%\")" \
	     --eval "(require 'asdf)" \
	     --eval "(push \"/Users/johnw/Library/Lisp/systems/\" \
			   asdf:*central-registry*)" \
	     --eval "(asdf:oos 'asdf:load-op :cambl)" \
	     --eval "(asdf:oos 'asdf:load-op :cambl-test)" \
	     --eval "(quit)"

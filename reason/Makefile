CC=ocamlc
REFMT=-pp "refmt --print binary"
OCAML_DEPS=str.cma
src=./src/native/
bin=./bin/

all: day2 day3 day4 day5

day%: $(src)adventOfCode%.re | create_dir
	@echo "Compiling $@"
	@$(CC) $(OCAML_DEPS) $(REFMT) -o $(bin)$@ -impl $<
	@cp $(src)day$*input $(bin) 2>/dev/null || exit 0

create_dir:
	@mkdir -p $(bin)

.PHONY: clean mrproper

clean:
	@rm -rf $(src)*.cmi $(src)*.cmo

mrproper: clean
	@rm -rf $(bin)

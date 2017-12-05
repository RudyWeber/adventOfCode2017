CC=ocamlc
REFMT=-pp "refmt --print binary"
OCAML_DEPS=str.cma
src=./src/native/
bin=./bin/

all: day2 day3 day4

day4: $(src)adventOfCode4.re | create_dir
	@echo "Compiling $@"
	@$(CC) $(OCAML_DEPS) $(REFMT) -o $(bin)$@ -impl $<
	@cp $(src)day4input $(bin)

day3: $(src)adventOfCode3.re | create_dir
	@echo "Compiling $@"
	@$(CC) $(OCAML_DEPS) $(REFMT) -o $(bin)$@ -impl $<

day2: $(src)adventOfCode2.re | create_dir
	@echo "Compiling $@"
	@$(CC) $(OCAML_DEPS) $(REFMT) -o $(bin)$@ -impl $<
	@cp $(src)day2input $(bin)

create_dir:
	@mkdir -p $(bin)

.PHONY: clean mrproper

clean:
	@rm -rf $(src)*.cmi $(src)*.cmo

mrproper: clean
	@rm -rf $(bin)

stack_build := "stack build --fast"
src_dirs := "src test"

# No default tasks
default:
  just --list

# Build and run tests
test:
  {{ stack_build }} --test

# Build only
build:
  {{ stack_build }} --test --no-run-tests

# Clean stack work
clean:
  stack clean --full

# Enter repl
ghci:
  stack ghci --test

# Open browser with generated docs
docs:
  stack haddock --open

# Install tool deps
deps:
  stack build --copy-compiler-tool hlint fourmolu

# Format with fourmolu
format:
  stack exec -- fourmolu --mode inplace {{ src_dirs }}

# Lint with hlint
lint:
  stack exec -- hlint {{ src_dirs }}

# Run the executable
exe:
  {{ stack_build }} --test --no-run-tests --exec overeasy-exe

# SLC Project Makefile

# Default target
.PHONY: all
all: test docs

# Run all tests
.PHONY: test
test:
	racket tests/main-tests.rkt

# Build HTML documentation
.PHONY: docs
docs: docs-html

.PHONY: docs-html
docs-html:
	mkdir -p docs/html
	raco scribble --html --dest docs/html scribblings/slc.scrbl

# Clean generated documentation
.PHONY: clean-docs
clean-docs:
	rm -rf docs/html

# Start the interactive REPL
.PHONY: repl
repl:
	racket slc/interactive.rkt

# Run performance demos
.PHONY: demo
demo:
	racket slc/performance-demo.rkt

# Build and open HTML documentation
.PHONY: docs-open
docs-open: docs-html
	@if command -v open >/dev/null 2>&1; then \
		open docs/html/slc.html; \
	elif command -v xdg-open >/dev/null 2>&1; then \
		xdg-open docs/html/slc.html; \
	else \
		echo "Documentation built in docs/html/slc.html"; \
	fi


# Help target
.PHONY: help
help:
	@echo "SLC Project Makefile"
	@echo "===================="
	@echo ""
	@echo "Common targets:"
	@echo "  all         - Run tests and build HTML docs"
	@echo "  test        - Run all test suites"
	@echo "  docs        - Build HTML documentation"
	@echo "  repl        - Start interactive REPL"
	@echo "  demo        - Run performance demonstrations"
	@echo "  clean-docs  - Clean generated documentation"
	@echo "  help        - Show this help message" 
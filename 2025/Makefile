.PHONY: run help

# Default target
help:
	@echo "Usage:"
	@echo "  make run N=<number>    - Run days/day<number>.hs"
	@echo ""
	@echo "Examples:"
	@echo "  make run N=00"

# Run a specific day's Haskell file
run:
	@if [ -z "$(N)" ]; then \
		echo "Error: Please specify N=<number>"; \
		echo "Example: make run N=00"; \
		exit 1; \
	fi
	@if [ ! -f "src/day$(N).hs" ]; then \
		echo "Error: src/day$(N).hs not found"; \
		exit 1; \
	fi
	@runhaskell src/day$(N).hs
UV            := uv

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

.PHONY: test
test: ## Runs test harness
	PYTHONPATH=. $(UV) run ruff check .
	PYTHONPATH=. $(UV) run mypy ./caten
	PYTHONPATH=. $(UV) run python -m pytest ./test

start:
	swipl app/server.pl --interactive --port 8080

# Help target to show available commands
help:
	@echo "Available commands:"
	@echo "  make start  - Start the development server on http://localhost:8080"
	@echo "  make stop   - Stop the development server"
	@echo "  make reload - Reload predicates without restarting server"
	@echo "  make help   - Show this help message"

.PHONY: start stop reload help

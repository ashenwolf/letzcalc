start:
	swipl -s app/server.pl -g 'server:start'

stop:
	swipl -s app/server.pl -g 'server:stop'

# Add reload command for development workflow  
reload: stop start

# Help target to show available commands
help:
	@echo "Available commands:"
	@echo "  make start  - Start the development server on http://localhost:8080"
	@echo "  make stop   - Stop the development server"
	@echo "  make reload - Reload predicates without restarting server"
	@echo "  make help   - Show this help message"

.PHONY: start stop reload help


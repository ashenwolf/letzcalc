# Lux Tax Calculator

## Disclaimer

This is just a playground to test the capabilities of SWI-Prolog in the area of website builds, so you should not use
it as a reliable source of calculation.

## Summary

This website is expected to be a one-stop-shop for various calculations related to life in Luxembourg

- Calculating the tax amount, with what-if analysis, for example:
  - what is the impact of inflation adjustments?
  - how much should I ask for Gross salary to have X Net
- Simple mortgage simulation with what-if analysis:
  - how much would the payments change if the base interest rate goes up/down
  - what is the impact of early repayment of variable cost part

## Development Workflow

### Starting the Development Server

**Primary Method:**
```bash
swipl -s app/server.pl -g 'server:start'
```

**Alternative (using Makefile):**
```bash
make start
```

The server runs on **http://localhost:8080**

### Making Changes

1. **Business Logic**: Edit files in `app/lib/`
   - Modify tax calculations, add new predicates
   - Restart server after changes (Prolog doesn't auto-reload)

2. **UI Components**: Edit files in `app/components/`
   - Modify HTML generation predicates
   - Restart server to see changes

3. **Page Controllers**: Edit files in `app/pages/`
   - Modify request handling and routing logic
   - Restart server after changes

4. **Styling**: Use only Tailwind CSS classes

### Testing Changes

1. **Manual Testing**: Use the web interface at localhost:8080
2. **Prolog Console**: Start SWI-Prolog and load modules manually
3. **Debug Mode**: Use Prolog debugging predicates for troubleshooting
4. **Reload Predicates**: To reload a changed predicate, run `make.` command in the SWI-Prolog console

### Available Commands

- `make start` - Start the development server
- `make stop` - Stop the development server  
- `make reload` - Reload predicates without restarting server
- `make help` - Show available commands

## Technical Details

- This website uses SWI-Prolog to run everything
- Frontend is built with:
  - Tailwind CSS for styling (via CDN)
  - TwinSpark for dynamic UI updates
- **Project Structure:**
  - `app/lib/` - Business logic (tax calculations, constants)
  - `app/components/` - UI components (HTML generation predicates)
  - `app/pages/` - Page controllers (request handling, routing)
  - `app/data/` - Data files (intervals.csv for tax brackets)
  - `app/api/` - API endpoints for AJAX/partial updates
  - `public/` - Static assets (JS files)
- Development server runs on port 8080

## Getting Started

1. Install SWI-Prolog (version 9.0 or higher)
2. Start the development server:
   ```bash
   make start
   # or alternatively:
   swipl -s app/server.pl -g 'server:start'
   ```
3. Open http://localhost:8080 in your browser

## Important Development Notes

- **Always reload predicates** after modifying Prolog files using `make.` in the SWI-Prolog console
- **Test thoroughly** - this is a tax calculator affecting real money decisions
- **Use only Tailwind CSS classes** for styling - no custom CSS
- **Check browser console** for JavaScript errors during development

## Author

Sergii Gulenok <sergii.gulenok@protonmail.com>

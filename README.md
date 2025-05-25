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


## Technical Details

- This website uses SWI-Prolog to run everything
- Frontend is built with:
  - Tailwind CSS for styling (via CDN)
  - Plain JavaScript for dynamic UI updates
- Project Structure:
  - `/app` - Main application directory
    - `/static` - Static assets (CSS, JS)
    - `/templates` - HTML templates
    - `/prolog` - Prolog source files
  - `/public` - Publicly accessible files
- API endpoints are available under `/partial` path
- Development server runs on port 8080

## Getting Started

1. Install SWI-Prolog (version 9.0 or higher)
2. Start the development server:
   ```bash
   swipl -s app/server.pl -g 'server:start'
   ```
3. Open http://localhost:8080 in your browser

## Author

Sergii Gulenok <sergii.gulenok@protonmail.com>

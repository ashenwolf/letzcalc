# Development Guide

## Quick Start

```bash
# Start the server
make start

# Server runs on http://localhost:8080
```

## Development Workflow

### 1. Making Changes

| Component | Location | Action After Change |
|-----------|----------|-------------------|
| **Business Logic** | `app/lib/` | Reload predicates with `make.` |
| **UI Components** | `app/components/` | Reload predicates with `make.` |
| **Page Controllers** | `app/pages/` | Reload predicates with `make.` |
| **Tax Data** | `app/data/intervals.csv` | Reload predicates with `make.` |

### 2. Reloading Changes

**Option 1: Quick Reload (Recommended)**
```bash
# In SWI-Prolog console
?- make.
```

**Option 2: Using Makefile**
```bash
make reload
```

**Option 3: Restart Server**
```bash
make stop
make start
```

### 3. Common Development Tasks

#### Adding New Tax Calculations
1. Edit `app/lib/tax.pl` - add new predicates
2. Update `app/lib/tax_constants.pl` if needed
3. Modify `app/data/intervals.csv` for new tax brackets
4. Update UI components to display new calculations
5. Run `make.` to reload

#### Adding New Pages
1. Create controller in `app/pages/`
2. Add route in `app/server.pl`
3. Create UI components in `app/components/`
4. Add navigation links if needed
5. Run `make.` to reload

#### Styling Guidelines
- **Use only Tailwind CSS classes**
- No custom CSS files
- Test responsive design (mobile, tablet, desktop)
- Use semantic HTML elements

### 4. Testing

1. **Manual Testing**: http://localhost:8080
2. **Browser Console**: Check for JavaScript errors
3. **Prolog Console**: Test predicates directly
4. **Debug Mode**: Use Prolog debugging predicates

### 5. Important Notes

⚠️ **Critical**: This is a tax calculator affecting real money decisions - test thoroughly!

- Always reload predicates after changes
- Check browser console for errors
- Test all tax calculation scenarios
- Verify responsive design works
- Use semantic HTML for accessibility

## File Structure Reference

```
app/
├── server.pl              # Main server & routing
├── lib/                   # Business logic
│   ├── tax.pl            # Tax calculations
│   └── tax_constants.pl  # Constants & config
├── components/           # UI components
│   ├── ui.pl            # Main UI components
│   └── header.pl        # Page headers
├── pages/               # Page controllers
│   ├── home.pl          # Home page
│   ├── tax.pl           # Tax calculator page
│   └── mortgage.pl      # Mortgage calculator page
├── api/                 # API endpoints
│   ├── tax.pl           # Tax calculation API
│   └── mortgage.pl      # Mortgage calculation API
└── data/               # Data files
    └── intervals.csv   # Tax brackets & rates
```

## Troubleshooting

| Problem | Solution |
|---------|----------|
| Changes not visible | Run `make.` in SWI-Prolog console |
| Server won't start | Check if port 8080 is available |
| JavaScript errors | Check browser console, verify TwinSpark |
| Styling issues | Verify Tailwind CSS classes are correct |
| Tax calculations wrong | Check `app/data/intervals.csv` and constants | 
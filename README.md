# COVID-19 Trend Tracker

A modern, interactive COVID-19 data visualization tool built with React and Plotly. Runs entirely in the browser with no server required.

## Features

- Real-time data from Our World in Data
- Global trends and multi-country comparisons
- Multiple metrics: total/new cases and deaths with per-capita normalization
- Interactive visualization with zoom, pan, and detailed hover information
- Rolling averages to smooth daily reporting variations
- Log/linear scale options for exponential growth visualization
- Fully client-side with no server dependencies
- GitHub Pages ready

## Quick Start

### Local Development

```bash
npm install
npm run dev
```

### Build for Production

```bash
npm run build
npm run preview
```

## Deployment to GitHub Pages

### Automated Deployment (Recommended)

1. Create a new GitHub repository
2. Upload project files
3. Create `.github/workflows/deploy.yml`:

```yaml
name: Deploy to GitHub Pages

on:
  push:
    branches: [ main ]

jobs:
  build-and-deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: '18'
      
      - name: Install dependencies
        run: npm install
      
      - name: Build
        run: npm run build
      
      - name: Deploy to GitHub Pages
        uses: peaceiris/actions-gh-pages@v3
        with:
          github_token: ${{ secrets.GITHUB_TOKEN }}
          publish_dir: ./dist
```

4. Enable GitHub Pages in repository Settings (Source: `gh-pages` branch)
5. Push to GitHub - automatic deployment will begin

### Manual Deployment

```bash
npm install
npm run build
npm install -g gh-pages
gh-pages -d dist
```

Access at: `https://[username].github.io/[repository-name]/`

## Technical Stack

- React 18
- Plotly.js
- Vite
- CSS3

## Data Source

Data fetched from [Our World in Data](https://ourworldindata.org/coronavirus) COVID-19 dataset. Note: Many countries reduced or stopped reporting after 2023.

## Usage Tips

- **Multiple countries**: Hold Ctrl/Cmd to select multiple countries for comparison
- **Log scale**: Better visualization for exponential growth patterns
- **Per capita**: Normalize data for comparing countries of different sizes
- **Rolling average**: Use 7-14 day averages to smooth reporting irregularities
- **Backdrop mode**: View all countries faintly in background for context

## License

MIT License

## Credits

Created by [Zeid Hamadeh](https://zhamadeh.github.io)  
Data: [Our World in Data](https://ourworldindata.org/coronavirus)

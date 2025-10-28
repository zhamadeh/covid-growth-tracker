# COVID-19 Trend Tracker - React Version

A modern, interactive COVID-19 data visualization tool built with React and Plotly. Runs entirely in the browser with no server required - perfect for GitHub Pages!

## ✨ Features

- **Real-time data**: Fetches latest COVID-19 statistics from Our World in Data
- **Multiple views**: Global trends or compare multiple countries
- **Flexible metrics**: Total/new cases and deaths with per-capita normalization
- **Interactive visualization**: Zoom, pan, and hover for detailed information
- **Rolling averages**: Smooth out daily reporting variations
- **Log/linear scales**: Better visualization for exponential growth
- **Beautiful design**: Modern, clean UI with professional color palette
- **Fully client-side**: No server needed, runs 100% in browser
- **GitHub Pages ready**: Easy deployment with no timeout issues

## 🎨 Improvements Over Shiny Version

### Fixed Issues:
1. ✅ **Clean log scale formatting** - No more messy decimals (0.001, 0.01)
2. ✅ **No data drop-off** - Lines end at last data point instead of dropping to zero
3. ✅ **Beautiful hover labels** - Clean, minimal tooltips without white rectangles
4. ✅ **Verified arithmetic** - Correct normalization and log scale calculations
5. ✅ **Better color palette** - Professional, distinct colors that look great

### New Features:
- **No timeout issues** - Runs entirely in browser, never disconnects
- **Faster performance** - No server round-trips
- **Modern design** - Clean, gradient header and card-based layout
- **Responsive** - Works on mobile, tablet, and desktop
- **Free hosting** - GitHub Pages is completely free

## 🚀 Deployment to GitHub Pages

### Method 1: Automated (Recommended)

1. **Create a new GitHub repository**
   ```bash
   # In your GitHub account, create a new repo called "covid-tracker"
   ```

2. **Upload these files to the repository**

3. **Enable GitHub Pages**
   - Go to repository Settings → Pages
   - Source: Deploy from a branch
   - Branch: `gh-pages` (will be created automatically)

4. **Set up GitHub Actions** (create `.github/workflows/deploy.yml`):
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

5. **Push to GitHub** - The site will automatically build and deploy!

### Method 2: Manual Deployment

1. **Install dependencies**
   ```bash
   npm install
   ```

2. **Build the project**
   ```bash
   npm run build
   ```

3. **Deploy the `dist` folder**
   ```bash
   # Install gh-pages package
   npm install -g gh-pages
   
   # Deploy
   gh-pages -d dist
   ```

4. **Access your site**
   ```
   https://[your-username].github.io/covid-tracker/
   ```

## 🛠️ Local Development

```bash
# Install dependencies
npm install

# Start development server
npm run dev

# Build for production
npm run build

# Preview production build
npm run preview
```

## 📊 Color Palette

The app uses a carefully selected color palette for better readability:

- **Blue** (#2563eb) - Primary
- **Red** (#dc2626) - Alert/High values
- **Green** (#16a34a) - Growth/Positive
- **Orange** (#ea580c) - Warning
- **Purple** (#7c3aed) - Secondary
- **Cyan** (#0891b2) - Cool tones
- **Pink** (#db2777) - Accent
- **Lime** (#84cc16) - Bright highlight
- **Amber** (#f59e0b) - Warm warning
- **Violet** (#8b5cf6) - Deep accent

## 📁 Project Structure

```
covid-tracker-react/
├── index.html          # Entry HTML
├── package.json        # Dependencies
├── vite.config.js      # Build configuration
└── src/
    ├── main.jsx        # React entry point
    ├── App.jsx         # Main application component
    └── App.css         # Styles
```

## 🔧 Technologies Used

- **React 18** - UI framework
- **Plotly.js** - Interactive charts
- **Vite** - Build tool (super fast!)
- **CSS3** - Custom styling with modern features

## 📝 Data Source

Data is fetched directly from [Our World in Data](https://ourworldindata.org/coronavirus) COVID-19 dataset. The app makes a single fetch request on load and processes all data client-side.

**Note**: Many countries reduced or stopped reporting after 2023, so recent data may be incomplete.

## 🎯 Usage Tips

1. **Comparing countries**: Select multiple countries (hold Ctrl/Cmd) to compare trends
2. **Log scale**: Use log scale for exponential growth visualization
3. **Per capita**: Enable normalization to compare countries of different sizes
4. **Rolling average**: Smooth out reporting irregularities with 7-14 day averages
5. **Backdrop mode**: See all countries faintly in background for context

## 🐛 Known Limitations

- Data completeness varies by country and time period
- Some countries stopped reporting after 2023
- CSV parsing is basic (works for OWID data structure)
- No offline support (requires internet to fetch data)

## 📜 License

MIT License - Feel free to use and modify!

## 👤 Author

Made by [Zeid Hamadeh](https://zhamadeh.github.io)  
Data: [Our World in Data](https://ourworldindata.org/coronavirus)

## 🙏 Acknowledgments

- Our World in Data for providing comprehensive COVID-19 data
- Plotly.js for excellent charting library
- React team for amazing framework

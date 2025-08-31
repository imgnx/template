const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');

// Get build target from environment variable or default to 'modular'
const buildTarget = process.env.BUILD_TARGET || 'modular';

const entryPoints = {
  bitcrusher: './src/bitcrusher.jsx',
  tuner: './src/tuner.jsx', 
  modular: './src/modular.jsx'
};

const pageConfigs = {
  bitcrusher: { title: 'Taku Bitcrusher', filename: 'index.html' },
  tuner: { title: 'Taku Universal Tuner', filename: 'index.html' },
  modular: { title: 'Taku Modular Audio Processing', filename: 'index.html' }
};

module.exports = {
  entry: entryPoints[buildTarget],
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: `bundle-${buildTarget}.js`,
    clean: true,
    publicPath: '/',
  },
  resolve: { extensions: ['.js', '.jsx'] },
  module: {
    rules: [
      { test: /\.(js|jsx)$/i, exclude: /node_modules/, use: 'babel-loader' }
    ]
  },
  devServer: {
    host: '0.0.0.0',
    port: 8080,
    hot: true,
    allowedHosts: 'all',
    historyApiFallback: true,
    proxy: { '/api': { target: 'http://localhost:8000', changeOrigin: true } }
  },
  plugins: [
    new HtmlWebpackPlugin({ 
      template: 'in/index.html',
      title: pageConfigs[buildTarget].title,
      filename: pageConfigs[buildTarget].filename
    })
  ]
};

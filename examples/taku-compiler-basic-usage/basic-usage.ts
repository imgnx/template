import { TakuCompiler } from '../../../taku/src/core/TakuCompiler';
import { DependencyResolverPlugin } from '../../../taku/src/plugins/DependencyResolverPlugin';
import { PythonBackendPlugin } from '../../../taku/src/plugins/PythonBackendPlugin';
import { ReactParserPlugin } from '../../../taku/src/plugins/ReactParserPlugin';
import { RustNativePlugin } from '../../../taku/src/plugins/RustNativePlugin';

async function main() {
  const compiler = new TakuCompiler({
    mode: 'development',
    react: { entry: './src/App.tsx', outputDir: './dist/frontend', typescript: true },
    python: { framework: 'fastapi', outputDir: './dist/backend', apiPrefix: '/api/v1' },
    rust: { outputDir: './dist/native', guiFramework: 'tauri', features: ['native-tls', 'system-tray'] },
    plugins: [new ReactParserPlugin(), new PythonBackendPlugin(), new RustNativePlugin(), new DependencyResolverPlugin()],
  });

  compiler.applyPlugins();

  compiler.hooks.reactComponentAnalyzed.tap('CustomAnalyzer', (component: any, analysis: any) => {
    console.log(`Custom analysis for ${component.name}:`, analysis);
  });

  compiler.hooks.beforeEmit.tapAsync('CustomOptimizer', async (_compilation: any, callback: Function) => {
    console.log('Running custom optimizations...');
    callback();
  });

  try {
    console.log('Starting Taku compilation...');
    await compiler.compile();
    console.log('Compilation completed successfully!');
  } catch (error) {
    console.error('Compilation failed:', error);
    process.exit(1);
  }
}

if (require.main === module) {
  main();
}

export { main };

import { resolve } from 'path'
import { defineConfig } from 'vite'
import elmPlugin from 'vite-plugin-elm'

export default defineConfig(({ command, mode }) => ({
  plugins: [elmPlugin.plugin(
    mode === 'production'
      ? { debug: false, optimize: true }
      : { debug: true, optimize: false })],
  build: {
    rollupOptions: {
      preserveEntrySignatures: 'strict',
      input: {
        index: resolve(__dirname, 'index.html'),
        server: resolve(__dirname, 'plank-server.js'),
      },
      output: {
        entryFileNames: `assets/[name].js`,
        chunkFileNames: `assets/[name].js`,
        assetFileNames: `assets/[name].[ext]`
      }
    },
  },
  server: {
    port: '2222'
  }
}));

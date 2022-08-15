import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";

export default defineConfig({
  base: "/where/",
  plugins: [elmPlugin({ debug: true, optimize: false })],
});
import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";

export default defineConfig({
  base: "/dice-digging/",
  plugins: [elmPlugin({ debug: false, optimize: false })],
});
import { defineConfig } from "vite";
import scalaJSPlugin from "@scala-js/vite-plugin-scalajs";

export default defineConfig({
  plugins: [scalaJSPlugin({ cwd: "../", projectID: "web" })],
  server: {
    proxy: {
      // For requests to /api/**, pass to backend
      "/api": {
        target: "http://localhost:8080",
      },
    },
  },
  base: "/stretch-the-kin",
});

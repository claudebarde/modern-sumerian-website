import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
// import { viteStaticCopy } from "vite-plugin-static-copy";

// https://vitejs.dev/config/
export default defineConfig({
  // root: "lib/es6/src",
  plugins: [
    react({
      include: ["**/*.res.mjs"]
    })
    // viteStaticCopy({
    //   targets: [
    //     {
    //       src: "/Users/claudebarde/Desktop/current_projects/modern-sumerian-website/src/**/*.scss",
    //       dest: "dest"
    //     },
    //     {
    //       src: "/Users/claudebarde/Desktop/current_projects/modern-sumerian-website/index.html",
    //       dest: "./lib/es6/src"
    //     }
    //   ]
    // })
  ]
});

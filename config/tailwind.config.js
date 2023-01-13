const colors = require("tailwindcss/colors");

module.exports = {
  content: ["./src/**/*.elm"],
  theme: {
    extend: {},
    fontFamily: {
      sans: ["'Inter'", "sans-serif"],
      heading: ["'Inter'", "sans-serif"],
    },
    colors: {
      turq: "#2BCBBA",
      white: colors.white,
      black: colors.black,
      grey: {
        0: "#f2f3f6",
        1: "#778CA34D",
        2: "#A5B1C2",
      },
    },
  },
  plugins: [],
};

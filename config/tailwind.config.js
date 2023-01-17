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
        1: "rgba(209, 216, 224, 0.3)",
        2: "#A5B1C2",
      },
    },
    boxShadow: {
      modal: "0 5px 30px -5px rgba(0, 0, 0, 0.15)",
    },
    backgroundImage: {
		"navblur": "url('./static/img/navblur.svg')"
	},
  },
  plugins: [],
};

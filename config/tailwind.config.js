module.exports = {
  content: ["./src/**/*.elm"],
  theme: {
    fontFamily: {
      sans: ["'Inter'", "sans-serif"],
      heading: ["'Inter'", "sans-serif"],
    },
    backgroundImage: {
      navblur: "url('./static/img/navblur.svg')",
    },
    extend: {
      colors: {
        turq: "#2BCBBA",
        grey: {
          0: "#f2f3f6",
          1: "rgba(209, 216, 224, 0.3)",
          2: "#A5B1C2",
        },
      },
      animation: {
        carousel: "carousel 17s linear infinite",
      },
      keyframes: {
        carousel: {
          from: { transform: "translateX(-50%)" },
          to: { transform: "translateX(0%)" },
        },
      },
      boxShadow: {
        modal: "0 5px 30px -5px rgba(0, 0, 0, 0.15)",
      },
    },
  },
  plugins: [],
};

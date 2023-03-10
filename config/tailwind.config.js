module.exports = {
  content: ["./src/**/*.elm"],
  theme: {
    fontFamily: {
      sans: ["'Inter'", "sans-serif"],
      heading: ["'Inter'", "sans-serif"],
    },
    backgroundImage: {
      navblur: "url('/static/img/navblur.svg')",
    },
    extend: {
      colors: {
        turq: "#2BCBBA",
        grey: {
          0: "#f2f3f6",
          1: "rgba(209, 216, 224, 0.3)",
          2: "#A5B1C2",
          3: "rgba(119, 140, 163, 0.3)",
        },
      },
      animation: {
        carousel: "carousel 17s linear infinite",
        errors: "errors 3s linear 1",
        spin: "spin 1s infinite ease",
        skeleton: "skeleton 0.6s linear infinite alternate",
      },
      keyframes: {
        carousel: {
          from: { transform: "translateX(-50%)" },
          to: { transform: "translateX(0%)" },
        },
        errors: {
          "0%": {
            transform: "translate(-50%, 0%) scale(0.8)",
            opacity: "0",
          },
          "5%, 75%": {
            transform: "translate(-50%, 0%) scale(1)",
            opacity: "1",
          },
          "95%, 100%": {
            transform: "translate(-50%, 200%) scale(0.8)",
            opacity: "0",
          },
        },
        skeleton: {
          from: {
            backgroundColor: "rgba(242, 243, 246, 1)",
          },
          to: {
            backgroundColor: "rgba(242, 243, 246, 0.4)",
          },
        },
      },
      boxShadow: {
        modal: "0 5px 30px -5px rgba(0, 0, 0, 0.15)",
        page: "0 5px 30px -5px rgba(0, 0, 0, 0.06)",
        portal: "0 20px 70px -4px rgba(209, 216, 224, 0.1)",
      },
    },
  },
  plugins: [],
};

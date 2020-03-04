new Vue({
  el: '#app',
  data: function () {
    let defaultSides = 10;
    let stats = Array(10);
    stats.fill(100);
    return {
      stats: stats,
      points: generatePoints(stats),
      sides: defaultSides,
      minRadius: 50,
      interval: null,
      updateInterval: 500
    };
  },
  watch: {
    sides: function (newSides, oldSides) {
      let sidesDifference = newSides - oldSides;
      if (sidesDifference > 0) {
        for (let i = 1; i <= sidesDifference; i++) {
          this.stats.push(this.newRandomValue());
        }
      } else {
        let absoluteSidesDifference = Math.abs(sidesDifference);
        for (let i = 1; i <= absoluteSidesDifference; i++) {
          this.stats.shift();
        }
      }
    },
    stats: function (newStats) {
      TweenLite.to(
        this.$data,
        this.updateInterval / 1000, {
          points: generatePoints(newStats)
        }
      );
    },
    updateInterval: function () {
      this.resetInterval();
    }
  },
  mounted: function () {
    this.resetInterval();
  },
  methods: {
    randomizeStats: function () {
      let vm = this;
      this.stats = this.stats.map(function () {
        return vm.newRandomValue();
      });
    },
    newRandomValue: function () {
      return Math.ceil(this.minRadius + Math.random() * (100 - this.minRadius));
    },
    resetInterval: function () {
      let vm = this;
      clearInterval(this.interval);
      this.randomizeStats();
      this.interval = setInterval(function () {
        vm.randomizeStats();
      }, this.updateInterval);
    }
  }
});

function valueToPoint(value, index, total) {
  let x = 0;
  let y = -value * 0.9;
  let angle = Math.PI * 2 / total * index;
  let cos = Math.cos(angle);
  let sin = Math.sin(angle);
  let tx = x * cos - y * sin + 100;
  let ty = x * sin + y * cos + 100;
  return {
    x: tx,
    y: ty
  };
}

function generatePoints(stats) {
  let total = stats.length;
  return stats.map(function (stat, index) {
    let point = valueToPoint(stat, index, total);
    return point.x + ',' + point.y;
  }).join(' ');
}
new Vue({
  el: '#message-box',
  data: {
    message: ''
  }
});

new Vue({
  el: '#example-1',
  data: {
    checkedNames: []
  }
});


new Vue({
  el: '#example-2',
  data: {
    picked: ''
  }
});

new Vue({
  el: '#example-3',
  data: {
    selected: ''
  }
});

new Vue({
  el: '#example-4',
  data: {
    selected: []
  }
});

new Vue({
  el: '#example-5',
  data: {
    selected: 'A',
    options: [{
        text: 'One',
        value: 'A'
      },
      {
        text: 'Two',
        value: 'B'
      },
      {
        text: 'Three',
        value: 'C'
      }
    ]
  }
});
Vue.component('todo-item', {
  template: `
  <li>
    {{ title }}
    <button @click="$emit('remove')">Remove</button>
  </li>
  `,
  props: ['title']
});

new Vue({
  el: '#todo-list-example',
  data: {
    todos: [{
        id: 1,
        title: 'Do the homework'
      },
      {
        id: 2,
        title: 'Anime'
      },
      {
        id: 3,
        title: 'Learn Vue'
      }
    ],
    newTodoText: '',
    nextTodoId: 4
  },
  methods: {
    addNewTodo: function () {
      this.todos.push({
        id: this.nextTodoId++,
        title: this.newTodoText
      });
      this.newTodoText = '';
    }
  }
});
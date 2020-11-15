import React from 'react';
import './App.css';
import Button from './Button';

function App() {
  return (
    <main>
      <div class="btn_container">
        <Button>default button</Button>
        <Button color="red">red button</Button>
        <Button color="black">black button</Button>
        <Button type="secondary">default button</Button>
        <Button type="secondary" color="red">red button</Button>
        <Button type="secondary" color="black">black button</Button>
      </div>
    </main>
  )
}

export default App;

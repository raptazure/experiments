const para1 = document.querySelector('p');
console.log(para1);

const para2 = document.querySelector('#p2');
console.log(para2);

const paras = document.querySelectorAll('p');
console.log(paras);

paras.forEach(para => {
  console.log(para);
});

document.querySelector('#p4').innerHTML = 'fourth p';
document.querySelector('#p3').textContent = 'third p';
paras[0].setAttribute('class', 'para');
paras[1].className = 'para';
paras[2].classList.add('para');

paras.forEach((para, i) => {
  para.style.fontFamily = 'fantasy';
  if (i % 2 == 0) {
    para.style.backgroundColor = 'red';
  } else para.style.backgroundColor = 'green';
});
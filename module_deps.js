let setColor = (color) => 
  (query) => {
    for (let n of document.querySelectorAll(query)) {
      //n.style.stroke = color;
      for (let p of n.querySelectorAll('polygon')) {
        p.style.stroke = color;
      }
      for (let p of n.querySelectorAll('path')) {
        p.style.stroke = color;
      }
    }
  };
let clear = () => setColor('')('*');
let highlight = (cn) => setColor('red')('.' + cn);

let _last_selected = '';
for (const n of document.getElementsByClassName('node')) {
  const cn = Array.from(n.classList).filter((s) => s != 'node');
  n.onclick = () => {
    if (_last_selected == cn) {
      clear();
      _last_selected = '';
    } else {
      clear();
      highlight(cn);
      _last_selected = cn;
    }
  };
}

window.addEventListener('load', (event) => {
  const score = document.querySelector('#score');
  const hand = document.querySelector('#input-hand');

  const createRegExp = (str, opts) => new RegExp(str.raw[0].replace(/\s/gm, ''), opts || '');

  const hand_re = createRegExp`
  ^(([1-9]+[mps])|
  ([1-7]+z)|
  ([1-9]*\([1-9]\)[1-9]*[mps])|
  ([1-7]*\([1-7]\)[1-7]*z)|
  ([1-9]*\([1-9][mps]\))|
  ([1-7]*\([1-7]z\))
  )*
  (\s(([1-9]+[mps][1-9]*)|
      ([1-7]+z[1-7]*)))*$`;

  const shorthandValid = (s) => {
    if (s.match(hand_re)) {
      if (s.indexOf(')') == -1 && s.indexOf('(') == -1) {
        return true;
      }
      else if (s.indexOf(')') == -1 || s.indexOf('(') == -1) {
        return false;
      }

      let open_paren_count = 0;
      for (const _ of s.matchAll(/\(/g)) {
        open_paren_count += 1;
      }
      let close_paren_count = 0;
      for (const _ of s.matchAll(/\)/g)) {
        close_paren_count += 1;
      }
      return open_paren_count == close_paren_count && open_paren_count == 1;
    }
    else {
      return false;
    }
  };

  const validMeld = (meld) => {
    const numbers = [];
    for (const c of meld) {
      if (c.match(/[1-9]/)) {
        numbers.push(Number(c));
      }
    }

    numbers.sort();
    if (numbers.length == 4) {
      return (new Set(numbers)).size == 1;
    }
    if (numbers.length == 3) {
      if ((new Set(numbers)).size == 1) {
        return true;
      }
      return (new Set([
        numbers[0] + 1,
        numbers[1],
        numbers[2] - 1])).size == 1;
    }
    return false;
  };

  const handFull = (s) => {
    const before_split = s.match(/^[^ ]*/)[0];
    const after_split_match = s.match(/ .*$/);
    const after_split = (after_split_match ? after_split_match[0] : ' ');

    //console.log('before:', before_split);

    let tiles_needed = 14;
    for (const c of before_split) {
      //console.log(c);
      if (c.match(/[1-9]/)) {
        tiles_needed--;
        //console.log('one less tile needed:', tiles_needed);
      }
    }

    //console.log('after:', '#', after_split, '#');
    for (const meld of after_split.split(' ')) {
      if (meld.match(/^\s*$/)) {
        continue;
      }
      //console.log(meld);
      if (validMeld(meld)) {
        tiles_needed -= 3;
        //console.log('three less tiles needed:', tiles_needed);
      }
      else {
        return false;
      }
    }

    return tiles_needed == 0;
  };

  const readGamestate = () => {
    const finish_type = document.querySelector('#finish-type').value;
    const seat = document.querySelector('#seat').value;
    const round = document.querySelector('#round').value;
    const opts = {
      riichi: document.querySelector('#riichi').checked,
      double: document.querySelector('#double').checked,
      ippatsu: document.querySelector('#ippatsu').checked,
      haitei: document.querySelector('#haitei').checked,
      houtei: document.querySelector('#houtei').checked,
      chankan: document.querySelector('#chankan').checked,
      rinshan: document.querySelector('#rinshan').checked,
      tenhou: document.querySelector('#tenhou').checked,
      chiihou: document.querySelector('#chiihou').checked,
    };
    const s2w = (s) => {
      if (s === '1z') {
        return 'e';
      }
      else if (s === '2z') {
        return 's';
      }
      else if (s === '3z') {
        return 'w';
      }
      else if (s === '4z') {
        return 'n';
      }
    };
    return [
      finish_type,
      'seat-' + s2w(seat),
      'round-' + s2w(round),
      ...Object.keys(opts).filter((k) => opts[k])];
  };
  score.addEventListener('click', (event) => {
    console.log('Hand:', hand.value);
    const hand_is_shorthand = shorthandValid(hand.value);
    console.log('valid shorthand:', hand_is_shorthand);
    if (hand_is_shorthand) {
      console.log('full:', handFull(hand.value));
      if (handFull(hand.value)) {
        window.location.href = `../score?hand=${hand.value}&gamestate=${readGamestate()}`;
      }
    }
    console.log('Hand invalid:', hand.value);
  });
});

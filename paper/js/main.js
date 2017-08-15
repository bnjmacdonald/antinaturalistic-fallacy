
var imgPath = "../output/figures";

window.onload = function() {
  insertReferences();
  insertDate();
  insertData(data);
  addFigurePath(imgPath);
}

// ADDS PATH TO FOLDER CONTAINING FIGURES
function addFigurePath(path) {
  var imgs = document.getElementsByClassName("needs-src");
  for(var i = 0; i < imgs.length; i++) {
    imgs[i].setAttribute('onerror', "this.src='http://placehold.it/150x150'");
    imgs[i].setAttribute('src', path + '/' + imgs[i].getAttribute('value'));
  }
  return (imgs);
}

// INSERTS DATA (e.g. from an R script)
// note: data should be loaded as a global variable from another script.
// var data = {
//   'a': 1,
//   'b': 'yes',
//   'c': 15.86744578
// };
function insertData(data) {
  var elements = document.getElementsByClassName('data');
  if (elements.length) {
    for(var i = 0; i < elements.length; i++) {
      id = elements[i].getAttribute('id');
      if (data.hasOwnProperty(id)) {
        console.log("Replacing innterHTML of #" + id + " with " + data[id]);
        elements[i].innerHTML = data[id];
      }
    }
  }
  return(elements);
}

// inserts references
function insertReferences() {
  var cites = document.querySelectorAll('cite');
  for(var i = 0; i < cites.length; i++) {
    // var theseCites = document.querySelectorAll(`a[href="#${cites[i].id}"]`);
    var theseCites = document.querySelectorAll("a[href=\"#" + cites[i].id + "\"]");
    for (var j = 0; j < theseCites.length; j++) {
      theseCites[j].innerHTML = cites[i].getAttribute('value');
    }
  }
}

function insertDate() {
  var monthNames = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
  element = document.getElementById('date');
  var date = new Date();
  var month = monthNames[date.getUTCMonth()];
  var day = date.getUTCDate();
  var year = date.getUTCFullYear();
  dateStr = month + " " + day + ", " + year;
  element.innerHTML = dateStr;
}


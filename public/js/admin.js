function ids(sortable, re) {
  return $(sortable).sortable("toArray").map(function(v){
    return v.replace(re, "")
  })
}

function highlighter(selector) {
  return function() {
    $(selector).effect("highlight", {color: "#4f4"})
  }
}

$(document).ready(function() {
  $(".collections").sortable({
    update: function() {
      $.post("/admin/collections/reorder",
             {slugs: ids(this, /^c-/)},
             highlighter(".collections"))
    }
  });

  $(".thumbs").sortable({
    update: function() {
      var c = $(this).
        sortable("widget").
        parent(".collection")[0].
        id.replace(/^c-/, "")

      $.post("/admin/collection/" + escape(c) + "/reorder",
             {slugs: ids(this, /^p-/)},
             highlighter(".thumbs li"))
    }
  });

  (function(){
    var hash = document.location.hash;
    setTimeout(function() { highlighter(hash)() }, 250)
  })()
})
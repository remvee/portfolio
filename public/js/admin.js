function ids(sortable, re) {
  return $(sortable).sortable("toArray").map(function(v){
    return v.replace(re, "")
  })
}

$(document).ready(function() {
  $(".collections").sortable({
    update: function() {
      var cids = ids(this, /^c-/)
      $.post("/admin/collections/reorder", {slugs: cids})
    }
  })

  $(".thumbs").sortable({
    update: function() {
      var c = $(this).sortable("widget").parent(".collection")[0]
      var cid = c.id.replace(/^c-/, "")
      var pids = ids(this, /^p-/)
      $.post("/admin/collection/" + cid + "/reorder", {slugs: pids})
    }
  })
})
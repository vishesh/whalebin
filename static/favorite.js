$(function() {
    $("#favorite").favorite_vote(
        function(succ) {
            var url = $("#favorite").attr('data-url');
            $.get("/ajax/star/state/" + url, function(data) {
                succ(data);
            });
        },
        function(succ) {
            var url = $("#favorite").attr('data-url');
            $.get("/ajax/star/set/" + url, function(data) {
                succ(data);
            });
        },
        function(succ) {
            var url = $("#favorite").attr('data-url');
            $.get("/ajax/star/unset/" + url, function(data) {
                succ(data);
            });
        }
    );
})

$(function() {
    // Source: http://jsfiddle.net/sdDVf/8/
    $("textarea").keydown(function(e) {
        if(e.keyCode === 9) { // tab was pressed
            // get caret position/selection
            var start = this.selectionStart;
            end = this.selectionEnd;

            var $this = $(this);

            // set textarea value to: text before caret + tab + text after caret
            $this.val($this.val().substring(0, start)
                    + "\t"
                    + $this.val().substring(end));

            // put caret at right position again
            this.selectionStart = this.selectionEnd = start + 1;

            // prevent the focus lose
            return false;
        }
    });

    // favorite stars
    $.fn.favorite_vote = function(get_state, on_star, on_unstar) {
        var that = this;

        that.append('<span class="favorite-star"></span>');
        that.append('<span>&nbsp;</span>');
        that.append('<span class="favorite-count"></span>');
        this.css("cursor", "pointer");
        this.css("font-size", "32px");

        function update_state(state) {
            var STAR = "★";
            var UNSTAR = "☆";
            
            var result;
            if (state.starred === true) {
                result = STAR;
            } else {
                result = UNSTAR;
            }
            that.find(".favorite-star").text(result);
            that.find(".favorite-count").text(state.count);
        }

        function upvote() {
            that.addClass("favorite-star-on");
            on_star(update_state);
        }
        
        function downvote() {
            that.removeClass("favorite-star-on");
            on_unstar(update_state);
        }
        
        that.click(function() {
            if (!that.hasClass("favorite-star-on")) {
                upvote();
            } else {
                downvote();
            }
        });

        get_state(update_state);
        return this;
    }

    function setCanvasMaxWidth() {
        var iframe = $("iframe.viewer")[0]
        var canvases = iframe.contentDocument.getElementsByTagName("canvas");

        if (canvases.length == 0) {
            console.log("no canvases found, try again");
            setTimeout(setCanvasMaxWidth, 1000); /* hack to resize to fit */
            return;
        }

        var maxWidth = 0;
        var maxHeight = 0;

        for (var i = 0; i < canvases.length; i++) {
            var canvas = canvases[i];
            if (canvas.width > maxWidth) {
                maxWidth = canvas.width;
            }
            if (canvas.height > maxHeight) {
                maxHeight = canvas.height;
            }
        }

        iframe.height = maxHeight + 100;
        iframe.width = maxWidth + 100;
    }

    setTimeout(setCanvasMaxWidth, 100);
});

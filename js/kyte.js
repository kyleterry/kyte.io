$(document).ready(function(){
    $('#blog-header').parallax('50%', 0.2);
    var distance = $(".post-header").offset().top;
    function headerSticker() {
        if($(window).scrollTop() >= distance) {
            $(".post-header").addClass("sticky-post-header");
            $(".post-body").addClass("marginize-post-body");
        } else {
            $(".post-header").removeClass("sticky-post-header");
            $(".post-body").removeClass("marginize-post-body");
        }
    }
    headerSticker();
    $(window).scroll(function() {
        headerSticker();
    });
});
$(document).foundation();
var doc = document.documentElement;
doc.setAttribute('data-useragent', navigator.userAgent);

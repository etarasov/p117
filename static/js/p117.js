$(document).ready(function () {

    var ajaxError = function (x,t,m) {
        alert('Server connection error');
    };

    function selectItem () {
        var pageId = this.getAttribute("data-pageid");
        $.ajax({
            type: "GET",
            url: "/mainpage/page",
            data: "pageId="+pageId,
            dataType: "json",
            error: ajaxError,
            success: function (ans) {
                var title = ans[0];
                var text = ans[1];
                $('#pageText').html("<h1>"+title+"</h1>"+text);
            },
        });
    };
    $("div.Content").click(selectItem);
})

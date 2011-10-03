$(document).ready(function () {

    var ajaxError = function (x,t,m) {
        alert('Server connection error');
    };

    function selectItem () {
        var pageId = this.getAttribute("data-pageid");
        var selectedItemId = $('#mainTree').attr("selectedItemId");
        if (pageId != selectedItemId) {
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

                    var selectedItemId = $('#mainTree').attr("selectedItemId");
                    $('div.Content[data-pageid="'+selectedItemId+'"]').removeClass("SelectedItem");

                    $('#mainTree').attr("selectedItemId", pageId);
                    $('div.Content[data-pageid="'+pageId+'"]').addClass("SelectedItem");
                },
            });
        }
    };
    $("div.Content").click(selectItem);
})

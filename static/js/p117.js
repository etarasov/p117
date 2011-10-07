$(document).ready(function () {

    function rebuttonButtons () {
        $("#editButton").button().click(editButtonHandler);
        $("#addButton").button().click(addButtonHandler);
    };

    var ajaxError = function (x,t,m) {
        alert('Server connection error');
    };

    function displaySelectedPage (pageId) {
        var res = "ok"
        $.ajax({
            type: "GET",
            url: "/mainpage/page",
            data: "pageId="+pageId,
            dataType: "html",
            error: function (x,t,m) {
                res = "fail";
                ajaxError(x,t,m);
            },
            success: function (ans) {
                $('#pageText').html(ans);
                rebuttonButtons();
            },
        });
        return res;
    };

    function clickTreeItem () {
        var pageId = this.getAttribute("data-pageid");
        selectTreeItem(pageId);
    }

    function selectTreeItem (pageId) {
        var selectedItemId = $('#mainTree').attr("data-selectedItemId");
        if (pageId != selectedItemId) {
            var selectedItemId = $('#mainTree').attr("data-selectedItemId");
            $('div.Content[data-pageid="'+selectedItemId+'"]').removeClass("SelectedItem");

            $('#mainTree').attr("data-selectedItemId", pageId);
            $('div.Content[data-pageid="'+pageId+'"]').addClass("SelectedItem");

            var res = displaySelectedPage(pageId);
        }
    };

    function reloadTree (pageToSelectId) {
        var pageId = pageToSelectId || $('#mainTree').attr("data-selectedItemId");
        var predicateId = $('#mainTree').attr("data-predicateid");
        saveTreeState();
        $('div#treeBlock').load('/mainpage/tree?predicateId='+predicateId, function () {
            restoreTreeState();
            selectTreeItem(pageId);
            $("div.Content").click(clickTreeItem);
        });
    }

    function saveTreeState () {
        var getIdState = function (index, element) {
            var id = $(element).attr("data-pageid");
            var liElem = $(element).parent();
            var state;
            if ($(liElem).hasClass('ExpandOpen')) {
                state = 'open';
            }
            if ($(liElem).hasClass('ExpandClosed')) {
                state = 'closed';
            }
            if ($(liElem).hasClass('ExpandLeaf')) {
                state = 'leaf';
            }
            return {id: id, state: state};
        };
        var itemsContent = $('#mainTree').find('div.Content');
        treeState = itemsContent.map(getIdState);
    };

    function restoreTreeState () {
        var restoreIdState = function (index, element) {
            var id = $(element).attr('data-pageid');
            var liElem = $(element).parent();
            for (var i=0; i < treeState.length; i++){
                if (treeState[i].id == id) {
                    var state = treeState[i].state;
                    if (state == "open") {
                        $(liElem).removeClass("ExpandClosed");
                        $(liElem).addClass("ExpandOpen");
                    }
                    if (state == "closed") {
                        $(liElem).removeClass("ExpandOpen");
                        $(liElem).addClass("ExpandClosed");
                    }
                }
            };
        };

        var itemsContent = $('#mainTree').find('div.Content');
        itemsContent.map(restoreIdState);
    };

    function editButtonHandler () {
        var selectedItemId = $('#mainTree').attr("data-selectedItemId");

        function submitPage () {
            var str = $("#editForm").serialize();
            // Save 'title' field to change it in tree
            var titleParam = $('#editForm > input[name="title"]').serializeArray();
            str = str + "&submit=Submit&pageId="+selectedItemId;
            $.ajax({
                type: "POST",
                url: "/mainpage/editpage",
                data: str,
                dataType: "json",
                error: ajaxError,
                success: function (ans) {
                    if (ans == "ok") {
                        displaySelectedPage(selectedItemId);
                        $('div.Content[data-pageid="'+selectedItemId+'"] > span.ItemText').html(titleParam[0].value);
                    }
                    else {
                    }
                },
            });

        };

        $.ajax({
            type: "GET",
            url: "/mainpage/editpage",
            data: "pageId="+selectedItemId,
            dataType: "html",
            error: ajaxError,
            success: function (ans) {
                $('#pageText').html(ans);
                $('#previewButton').button();
                $('#submitButton').button().click(submitPage);
            },
        });
    };

    function addButtonHandler () {
        var selectedItemId = $('#mainTree').attr("data-selectedItemId");
        var predicateId = $('#mainTree').attr("data-predicateId");

        // Get id of page parent in the tree. It's used when create new page at the same level as selected page.
        var parentSelectedItemId = $($($($('div.Content[data-pageid="'+selectedItemId+'"]').parent()).parent()).prev()).attr("data-pageid") || -1;

        function submitPage () {
            var str = $("#addForm").serialize();
            str = str + "&submit=Submit&pageId="+selectedItemId+"&predicateId="+predicateId+"&parentId="+parentSelectedItemId;
            $.ajax({
                type: "POST",
                url: "/mainpage/addpage",
                data: str,
                dataType: "json",
                error: ajaxError,
                success: function (ans) {
                    if (ans[0] == "ok") {
                        var pageId = ans[1];
                        // reload tree
                        reloadTree(pageId);
                    }
                    else
                    {
                        alert('Ошибка добавления страницы:'+ans);
                    }
                },
            });
        };

        $.ajax({
            type: "GET",
            url: "/mainpage/addpage",
            data: "pageId="+selectedItemId+"&predicateId="+predicateId,
            dataType: "html",
            error: ajaxError,
            success: function (ans) {
                $("#pageText").html(ans);
                $('#previewButton').button();
                $('#submitButton').button().click(submitPage);
            }
        });
    };

    function selectFirstItem() {
        var pageToSelect = $($($('span.ItemText').first()).parent()).attr("data-pageid");
        if (pageToSelect) {
            selectTreeItem(pageToSelect);
        }
        else {
            alert("there are no pages in the predicate yet");
        }
    };


    var treeState = 0;
    $("div.Content").click(clickTreeItem);
    selectFirstItem();



    $('#testButton').button().click( function () {
        reloadTree();
    });
})

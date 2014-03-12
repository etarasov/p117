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

    function getPathForNode (node) {
        var pathBegin = node.data.pageId;
        var res;

        if (node.getLevel() == 0) {
            alert("Trying to get path for root node");
        }

        if (node.getLevel() == 1) {
            res = pathBegin;
        }
        else {
            parentNode = node.getParent();
            res = getPathForNode(parentNode) + ";" + pathBegin;
        }

        return res;
    }

    function getNodeForPath(root, path) {
        var res = null;
        if (path == "") {
            res = root;
        }
        else {
            var pathElems = path.split(";");
            var rootElem = pathElems.shift();

            var children = root.getChildren();
            for (var i=0; i < children.length; i++) {
                if (children[i].data.pageId == rootElem) {
                    res = getNodeForPath(children[i], pathElems.join(";"));
                    break;
                }
            }
        }

        return res;
    }

    function editButtonHandler () {
        var path = $('#treeContainer').attr("data-selectedpath");
        var selectedItemId = path.split(";")[path.split(";").length - 1];

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
                        $('#mainTree').dynatree("getTree").reload();
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
        var predicateId = $('#treeContainer').attr("data-predicateId");

        var path = $('#treeContainer').attr("data-selectedpath");
        var selectedItemId = path.split(";")[path.split(";").length - 1];

        // Get id of page parent in the tree. It's used when create new page at the same level as selected page.
        // TODO: it's a code for previous js tree widget
        //var parentSelectedItemId = $($($($('div.Content[data-pageid="'+selectedItemId+'"]').parent()).parent()).prev()).attr("data-pageid") || -1;

        var selectedNode = $('#mainTree').dynatree('getTree').getActiveNode();
        var selectedNodeParent = selectedNode.getParent();
        var selectedNodeParentPageId = selectedNodeParent.data.pageId;

        function submitPage () {
            var str = $("#addForm").serialize();
            str = str + "&submit=Submit&pageId="+selectedItemId+"&predicateId="+predicateId+"&parentId="+selectedNodeParentPageId;
            $.ajax({
                type: "POST",
                url: "/mainpage/addpage",
                data: str,
                dataType: "json",
                error: ajaxError,
                success: function (ans) {
                    if (ans[0] == "ok") {
                        var pageId = ans[1];
                        $('#mainTree').dynatree("getTree").reload();
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

    function changePredicateSelectHandler () {
        var oldPredicate = $('#treeContainer').attr("data-predicateid");
        var newPredicate = $('#predicateSelect').val();

        if (oldPredicate != newPredicate) {
            window.location.pathname = "/mainpage?predicateId="+newPredicate;
        }
    };

    //^^^^^^^^^^^^^ Only functions definitions ^^^^^^^^^^^^^^^^^^^^^

    var treeState = 0;
    //$("div.Content").click(clickTreeItem);

    var loadedPredicateId = $('#treeContainer').attr("data-predicateid");

    $('#predicateSelect').change(changePredicateSelectHandler);


    $('#mainTree').dynatree({
        onActivate: function(node) {
            if(node.tree.isUserEvent()){
                var path = getPathForNode(node);
                window.location.pathname = "/mainpage?Path="+path+"&predicateId="+loadedPredicateId;
            }
            else {
                displaySelectedPage(node.data.pageId);
            }
        },
        initAjax: {
            url: "/mainpage/tree",
            data: {
                predicateId: loadedPredicateId
            },
        },
        cookieId: "117_maintree_" + loadedPredicateId,
        onPostInit: function(isReloading, isError) {
            if(!isReloading) {
                this.visit(function(n) {
                    n.expand(true);
                });
            }

            // 1. Get path

            var path = $('#treeContainer').attr("data-selectedpath");

            // if path is empty, look it up in cookies

            if (typeof path === "undefined" || path == "") {
                path = $.cookie('path_for_'+loadedPredicateId);
                window.location.pathname = "/mainpage?Path="+path+"&predicateId="+loadedPredicateId;
            }
            else {
                var node = getNodeForPath(this.getRoot(), path);

                // 3. TODO: Try to find first real node if necessary

                // 4. Activate node

                var activeNode = this.getActiveNode();

                if (activeNode == null || activeNode.data.key != node.data.key) {
                    this.activateKey(node.data.key);
                }
                else {
                    this.reactivate();
                }

                // 5. Set cookie

                $.cookie('path_for_'+loadedPredicateId, path, { expires: 30 });
            }
        },
        persist: true
    });
})

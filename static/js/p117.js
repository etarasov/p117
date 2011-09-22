$(document).ready(function () {
    // http://javascript.ru/ui/ajaxtree
    function tree (id, url) {
        var element = $('#mainTree');

        function hasClass(elem, className) {
            return new RegExp("(^|\\s)"+className+"(\\s|$)").test(elem.className)
        }

        function toggleNode(node) {
            var newClass = hasClass(node, 'ExpandOpen') ? 'ExpandClosed' : 'ExpandOpen'
            var re = /(^|\s)(ExpandOpen|ExpandClosed)(\s|$)/
            node.className = node.className.replace(re, '$1'+newClass+'$3')
        }
        function load(node) {
            alert(load)
        }
        element.onclick = function (event) {
            event = event || window.event
            var clickedElem = event.target || event.srcElement

            if (!hasClass(clickedElem, 'Expand')) {
                return
            }

            var node = clickedElem.parentNode
            if (hasClass(node, 'ExpandLeaf')) {
                return
            }

            if (node.isLoaded || node.getElementByTagName('LI').length) {
                toggleNode(node)
                return
            }

            if (node.getElementsByTagName('LI').length) {
                toggleNode(node)
                return
            }

            load(node)
        }
    }

    alert("notree");
})

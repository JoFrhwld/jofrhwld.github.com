var labelType, useGradients, nativeTextSupport, animate;

(function() {
  var ua = navigator.userAgent,
      iStuff = ua.match(/iPhone/i) || ua.match(/iPad/i),
      typeOfCanvas = typeof HTMLCanvasElement,
      nativeCanvasSupport = (typeOfCanvas == 'object' || typeOfCanvas == 'function'),
      textSupport = nativeCanvasSupport 
        && (typeof document.createElement('canvas').getContext('2d').fillText == 'function');
  //I'm setting this based on the fact that ExCanvas provides text support for IE
  //and that as of today iPhone/iPad current text support is lame
  labelType = (!nativeCanvasSupport || (textSupport && !iStuff))? 'Native' : 'HTML';
  nativeTextSupport = labelType == 'Native';
  useGradients = nativeCanvasSupport;
  animate = !(iStuff || !nativeCanvasSupport);
})();

var Log = {
  elem: false,
  write: function(text){
    if (!this.elem) 
      this.elem = document.getElementById('log');
    this.elem.innerHTML = text;
    this.elem.style.left = (500 - this.elem.offsetWidth / 2) + 'px';
  }
};


function init(){
    //init data
    var json = {
        id: "data",
        name: "data[i]",
        data: {width: 100},
        children: [
            {id: "normal",
             name: "normal",
             data: {nodeType: "dist"},
             children: [
                {id: "mu",
                 name: "&mu;",
                 data: {nodeType: "param"},
                 children: [
                    {id: "sum", name: "sum", data: {nodeType: "function"}, 
                    children:[
                        {id: "speaker", name: "speaker", data: {nodeType: "param"}, children:[
                            {id: "normal1", name: "normal", data: {nodeType: "dist"}, children:[
                                {id: "mu1", name: "&mu;|speaker & context", data: {nodeType: "param", width:100}, children:[]},
                                {id: "tau1", name: "&tau;|speaker", data: {nodeType: "param", width:100}, children:[]}
                            ]}
                        ]},
                        {id: "word", name: "word", data: {nodeType: "param"}, children:[
                            {id: "normal2", name: "normal", data: {nodeType: "dist"}, children:[
                                {id: "mu2", name: "&mu;|word", data: {nodeType: "param"}, children:[]},
                                {id: "tau2", name: "&tau;|word", data: {nodeType: "param"}, children:[]}
                                
                            ]}
                        ]},
                        {id: "duration", name: "duration", data: {nodeType: "param"}, children:[
                            {id: "normal3", name: "normal", data: {nodeType: "dist"}, children:[
                                {id: "mu3", name: "&mu;", data: {nodeType: "param"}, children:[
                                    {id: "sum1", name: "sum", data: {nodeType: "function"}, children:[
                                        {id: "intercept", name: "intercept", data: {nodeType: "param"}, children:[
                                            {id: "normal5", name: "normal", data: {nodeType: "dist"}, children:[
                                                {id: "mu5", name: "&mu;=0", data: {nodeType: "param"}, children:[]},
                                                {id: "tau5", name: "&tau;=0.001", data: {nodeType: "param"}, children:[]}
                                            ]}
                                        
                                        ]},
                                        {id: "product", name: "product", data: {nodeType: "function"}, children:[
                                            {id: "slope", name: "slope", data: {nodeType: "param"}, children:[
                                                {id: "normal4", name: "normal", data: {nodeType: "dist"}, children:[
                                                    {id: "mu4", name: "&mu;=0", data: {nodeType: "param"}, children:[]},
                                                    {id: "tau4", name: "&tau;=0.001", data: {nodeType: "param"}, children:[]}
                                                ]}
                                            ]},
                                            {id: "dur", name: "duration[i]", data: {}, children:[]}
                                        ]}
                                    ]}
                                ]},
                                {id: "tau3", name: "&tau;", data: {nodeType: "param"}, children:[
                                    {id: "gamma1", name: "gamma", data: {nodeType: "dist"}, children:[
                                        {id: "rate1", name: "rate=0.001", data: {nodeType: "param"}, children:[]},
                                        {id: "shape1", name: "shape=0.001", data: {nodeType: "param"}, children:[]}
                                    ]}
                                ]}
                            ]}
                        ]}
                    ]}
                 ]},
                {id: "tau",
                 name: "&tau;",
                 data: {nodeType: "param"},
                 children: [
                    {id: "gamma",
                     name: "gamma",
                     data: {nodeType: "dist"},
                     children: [
                        {id: "shape", 
                         name: "shape = 0.001",
                         data: {nodeType: "param", width: 100},
                         children:[]},
                         {id: "rate",
                          name: "rate = 0.001", 
                          data: {nodeType: "param", width: 100}, children:[]}
                     ]}
                 ]}
                ]}
             ]};
    //end
    //init Spacetree
    //Create a new ST instance
    var st = new $jit.ST({
        //id of viz container element
        injectInto: 'infovis',
        //set duration for the animation
        duration: 400,
        //set animation transition type
        transition: $jit.Trans.Quart.easeInOut,
        //set distance between node and its children
        levelDistance: 50,
        //enable panning
        Navigation: {
          enable:true,
          panning:true
        },
        //set node and edge styles
        //set overridable=true for styling individual
        //nodes or edges
        Node: {
            height: 20,
            width: 60,
            type: 'rectangle',
            color: '#aaa',
            overridable: true
        },
        
        Edge: {
            type: 'revarrow',
            overridable: true
        },
        
        onBeforeCompute: function(node){
            Log.write("loading " + node.name);
        },
        
        onAfterCompute: function(){
            Log.write("done");
        },
        
        //This method is called on DOM label creation.
        //Use this method to add event handlers and styles to
        //your node.
        onCreateLabel: function(label, node){
            label.id = node.id;            
            label.innerHTML = node.name;
            label.onclick = function(){
                if(normal.checked) {
                  st.onClick(node.id);
                } else {
                st.setRoot(node.id, 'animate');
                }
            };
            label.data = node.data;
            //set label styles
            var style = label.style;
            if(label.data.width){
                style.width = label.data.width + "px";
            }else{
                style.width = 60 + 'px';
            }
            style.height = 17 + 'px';            
            style.cursor = 'pointer';
            style.color = '#000000';
            style.fontSize = '0.8em';
            style.textAlign= 'center';
            style.paddingTop = '3px';
        },
        
        //This method is called right before plotting
        //a node. It's useful for changing an individual node
        //style properties before plotting it.
        //The data properties prefixed with a dollar
        //sign will override the global node style properties.
        onBeforePlotNode: function(node){
            //add some color to the nodes in the path between the
            //root node and the selected node.
            if (node.selected) {
                node.data.$color = "#FFD865";
            }
            else {
//                node.data.$color = "#63AFE5";
                //if the node belongs to the last plotted level
                //if(!node.anySubnode("exist")) {
                    //count children number
                var count = 0;
                node.eachSubnode(function(n) { count++; });
                //    //assign a node color based on
                //   //how many children it has
                //node.data.$color = [ "#63AFE5", '#206595'][count];
                if(count==0){
                    node.data.$color = '#206595';
                }
                else{
                    node.data.$color = "#63AFE5";
                }
                //}
            }
            if(node.data.width){
                node.data.$width = node.data.width;
            }
            if(node.data.nodeType == "dist"){
                node.data.$type = "ellipse";
            }
            if(node.data.nodeType == "function"){
                node.data.$type = "rectangle";
            }

        },
        
        //This method is called right before plotting
        //an edge. It's useful for changing an individual edge
        //style properties before plotting it.
        //Edge data proprties prefixed with a dollar sign will
        //override the Edge global style properties.
        onBeforePlotLine: function(adj){
            if (adj.nodeFrom.selected && adj.nodeTo.selected) {
                adj.data.$color = "#FFD865";
                adj.data.$lineWidth = 3;
            }
            else {
                delete adj.data.$color;
                delete adj.data.$lineWidth;
            }
            if(adj.nodeTo.data.nodeType == "param"){
                adj.data.$type = "line";
            }
            if(adj.nodeFrom.data.nodeType == "function"){
                adj.data.$type = "line";
            }
            if(adj.nodeFrom.data.nodeType == "dist"){
                adj.data.$type = "line";
            }

            if(adj.nodeTo.data.nodeType == "dist"){
                adj.data.$type = "revlab";
            }

        }
    });
    //load json data
    st.loadJSON(json);
    //compute node positions and layout
    st.compute();
    //optional: make a translation of the tree
    st.geom.translate(new $jit.Complex(-200, 0), "current");
    //emulate a click on the root node.
    st.onClick(st.root);
    //end
    //Add event handlers to switch spacetree orientation.
    var top = $jit.id('r-top'), 
        left = $jit.id('r-left'), 
        bottom = $jit.id('r-bottom'), 
        right = $jit.id('r-right'),
        normal = $jit.id('s-normal');
        
    
    function changeHandler() {
        if(this.checked) {
            top.disabled = bottom.disabled = right.disabled = left.disabled = true;
            st.switchPosition(this.value, "animate", {
                onComplete: function(){
                    top.disabled = bottom.disabled = right.disabled = left.disabled = false;
                }
            });
        }
    };
    
    top.onchange = left.onchange = bottom.onchange = right.onchange = changeHandler;
    //end

}

// Create NGL Stage object
var stage = new NGL.Stage( "viewport" );

// Handle window resizing
window.addEventListener( "resize", function( event ){
    stage.handleResize();
}, false );


// Load PDB entry 1IZL, show a ball and stick representation
// of pheophytin (PHE) in chain A (:A) and focus on it (.autoView)
stage.loadFile( "rcsb://1IZL" ).then(function(comp){
    comp.addRepresentation('ball+stick', {name:"pho", sele: 'PHO and :A' })
    comp.autoView('PHO and :A')
});

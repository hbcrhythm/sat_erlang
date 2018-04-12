
-define('#vector', #{
		mod 	=> vector	%% mod tag
		,x  	=> 0
		,y  	=> 0
	}).

-define('#circle', #{
		mod 	=> circle
		,pos 	=> ?'#vector'
		,r 		=> 0
	}).

-define('#box', #{
		mod 	=> box
		,w	 	=> 0
		,h		=> 0
	}).

-define('#polygon', #{
		mod 		=> polygon
		,pos 		=> ?'#vector'
		,angle 		=> 0
		,offset 	=> ?'#vector'
		,points 	=> []
		,calcPoints => []
		,edges	 	=> []
		,normals 	=> []
		
	}).

-define('#response', #{
		mod 		=> response
		,a 			=> 0
		,b   		=> 0
		,overlapN 	=> ?'#vector'
		,overlapV 	=> ?'#vector'
		,aInB  		=> false
		,bInA		=> false
		,overlap	=> 1 bsl 32
		,isCollide  => false
	}).

-define(LEFT_VORONOI_REGION, -1).
-define(MIDDLE_VORONOI_REGION, 0).
-define(RIGHT_VORONOI_REGION, 1).

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
		,offset 	=> []
		,points 	=> []
		,calcPoints => []
		,edges	 	=> []
		,normals 	=> []
		
	}).
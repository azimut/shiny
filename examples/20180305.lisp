(in-package :somecepl)

#|


void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
  vec2 uv=fragCoord.xy/iResolution.xy-.5;
  uv.y*=iResolution.y/iResolution.x;
  vec3 dir=vec3(uv*zoom,1.);
  float time=iTime*speed+.25;
  vec3 from=vec3(1.,.5,0.5);
  from+=vec3(time*2.,time,-2.);	
  float s=0.1,fade=1.;
  vec3 v=vec3(0.);
  for (int r=0; r<volsteps; r++) {
    vec3 p=from+s*dir*.5;
    p = abs(vec3(tile)
            -mod(p,vec3(tile*2.)));
    float pa,a=pa=0.;
    for (int i=0; i<iterations; i++) { 
      p=abs(p)/dot(p,p)
             -formuparam; 
      a+=abs(length(p)-pa);
      pa=length(p);
    }
    float dm=max(0.,darkmatter-a*a*.001); //dark matter
    a*=a*a; // add contrast
    v+=vec3(s,s*s,s*s*s*s)
                *a
                *brightness
                *fade;
  }
	fragColor = vec4(v*.01,1.);	
}
// Star Nest by Pablo RomÃ¡n Andrioli

// This content is under the MIT License.

#define iterations 17
#define formuparam 0.53

#define volsteps 20
#define stepsize 0.1

#define zoom   0.800
#define tile   0.850
#define speed  0.010 

#define brightness 0.0015
#define darkmatter 0.300
#define distfading 0.730
#define saturation 0.850


void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
	//get coords and direction
	vec2 uv=fragCoord.xy/iResolution.xy-.5;
	uv.y*=iResolution.y/iResolution.x;
	vec3 dir=vec3(uv*zoom,1.);
	float time=iTime*speed+.25;
	vec3 from=vec3(1.,.5,0.5);
	from+=vec3(time*2.,time,-2.);	
	//volumetric rendering
	float s=0.1,fade=1.;
	vec3 v=vec3(0.);
	for (int r=0; r<volsteps; r++) {
		vec3 p=from+s*dir*.5;
		p = abs(vec3(tile)
                        -mod(p,vec3(tile*2.)));
                //<- tiling fold
		float pa,a=pa=0.;
		for (int i=0; i<iterations; i++) { 
			p=abs(p)/dot(p,p)
                          -formuparam; 
                        //<- the magic formula
			a+=abs(length(p)-pa); // absolute sum of average change
			pa=length(p);
		}
		float dm=max(0.,darkmatter-a*a*.001); //dark matter
		a*=a*a; // add contrast
		if (r>6) fade*=1.-dm; // dark matter, don't render near
		//v+=vec3(dm,dm*.5,0.);
		v+=fade;
		v+=vec3(s,s*s,s*s*s*s)
                   *a
                   *brightness
                   *fade;
                // coloring based on distance
		fade*=distfading; // distance fading
		s+=stepsize;
	}
	v=mix(vec3(length(v)),v,saturation); //color adjust
	fragColor = vec4(v*.01,1.);	
	
}
|#
(defclass actor ()
  ((pos :initform (v! 0 0 0) :initarg :pos)
   (rot :initform 0f0 :initarg :rot)
   (scale :initform 1f0 :initarg :scale)
   (rows :initform 1f0 :initarg :rows)
   (speed :initform 1f0 :initarg :speed)
   (width :initform 1f0 :initarg :width)
   (columns :initform 1f0 :initarg :columns)
   (visual :initarg :visual)))

(defvar *screen-height* 1f0)

(defvar *actors* nil)

(defun mynow ()
  ;; Just some number that increases over time that we use
  ;; in a bunch of places
  (/ (get-internal-real-time)
     1000f0))
(defvar *gpu-verts-arr* nil)
(defvar *gpu-index-arr* nil)
(defvar *vert-stream* nil)
;; --------------------------------------------------
;;(defvar *cube-stream* nil)
(defvar *cam-pos* (v! 0 0 0))

(defvar *blend* (make-blending-params))

;; --------------------------------------------------
(defvar *samplers* (make-hash-table :test #'equal))
(defun load-tex (rel-path)
  (or (gethash rel-path *samplers*)
      (let* ((path (asdf:system-relative-pathname :somecepl rel-path))
             (tex  (load-image-to-texture path)))
        (setf (gethash rel-path *samplers*)
              (sample tex)))))
;; --------------------------------------------------
(defun-g vert ((vert :vec2) &uniform
               (scale :float)
               (pos :vec3)
               (width :float)
               (screen-height :float)
               (screen-ratio :float))
  (values (v! (/ (+ (s~ pos :xy)
                    (* scale vert))
                 (v! (* screen-ratio (/ 1f0 width))
                     screen-height))
              0 1)
          (+ (v2! .5) (* vert .5))))

(defun-g frag ((uv :vec2)
               &uniform
               (speed :float)
               (rows :float)
               (columns :float)
               (time :float)
               (resolution :vec2)
               (sam :sampler-2d)
               (cam-pos :vec3))
  ;; 8 = width size
  ;; time ...mod does not matter...
  ;; .125 = 1/8
  ;; -1 ...flip vertically
  ;; /2 ... pick one row of the two
  (let* (
         (time (* speed time))
         (mtime (if (equal columns 1)
                    (mod time columns)
                    (floor (mod time columns))))
         (vx (+ (* (* -1 mtime)
                   (/ 1f0 columns))
                (/ (x uv)
                   columns)))
         (vy (/ (* -1 (y uv))
                rows))
;;;         (vx (* (cos time) vx))
;;;         (vy (* (cos time) vy))
         ;;; wibble
         (t-uv (v! vx vy))
         (color (texture sam t-uv))
         )
    color))
;; --------------------------------------------------
(defpipeline-g mario ()
  :vertex (vert :vec2)
  :fragment (frag :vec2))

(defun init ()
  (setf *screen-width* 1f0)
  ;;  (setf (clear-color) (v! .46 .57 .69 1))
  (setf (clear-color) (v! 0 0 0 1))
  (setf *actors* nil)
  (unless *actors*
    (setf *actors*
          (make-array 0 :element-type 'actor
                      :adjustable t
                      :fill-pointer 0))
    (vector-push-extend
     (make-instance
      'actor
      :visual (load-tex "parallax-forest-front-trees.png")
      :columns 1f0
      :speed .5f0
      :width 2f0
      :rows 1f0) *actors*)
    (vector-push-extend
     (make-instance
      'actor
      :visual (load-tex "scottpilgrim_multiple.png")
      :columns 8f0
      :scale .3f0
      :speed 7f0
      :pos (v! 0 -.6 0)
      :rows 2f0) *actors*))
  (when *gpu-verts-arr*
    (free *gpu-verts-arr*))
  (when *gpu-index-arr*
    (free *gpu-index-arr*))
  (when *vert-stream*
    (free *vert-stream*))
  
  (setf *gpu-verts-arr*
        (make-gpu-array
         (list (v! -1.0  1.0)
               (v! -1.0 -1.0)
               (v!  1.0 -1.0)
               (v!  1.0  1.0))
         :element-type :vec2))
  
  (setf *gpu-index-arr*
        (make-gpu-array
         (list 0 1 2
               0 2 3)))

  (setf *vert-stream*
        (make-buffer-stream *gpu-verts-arr*
                            :index-array *gpu-index-arr*)))

(defun draw! ()
  (step-host)
  (clear)
  (let ((res (surface-resolution (current-surface))))
    (setf (resolution (current-viewport))
          res)
    (with-setf* ((depth-test-function) nil)
      (with-blending *blend*
        (loop :for x :across *actors* :do
           (with-slots (rows speed width columns pos scale visual) x
             (map-g #'mario *vert-stream*
                    :time (mynow)
                    :rows rows
                    :speed speed
                    :columns columns
                    :scale scale
                    :screen-height *screen-height*
                    :width width
                    :screen-ratio (/ (x res) (y res))
                    :pos pos
                    :resolution res
                    :sam visual
                    :cam-pos *cam-pos*))))))
  (swap)
  )

(def-simple-main-loop runplay (:on-start #'init)
  (draw!))

(in-package :shiny)

#|
float sphereSDF(vec3 p) {
    return length(p) - 1.0;
}
|#

#|
/**
 * Part 1 Challenges
 * - Make the circle yellow
 * - Make the circle smaller by decreasing its radius
 * - Make the circle smaller by moving the camera back
 * - Make the size of the circle oscillate using the sin() function and the iTime
 *   uniform provided by shadertoy
 */

const int MAX_MARCHING_STEPS = 255;
const float MIN_DIST = 0.0;
const float MAX_DIST = 100.0;
const float EPSILON = 0.0001;
|#

;; /**
;;  * Signed distance function for a sphere centered at the origin with radius 1.0;
;;  */
;; float sphereSDF(vec3 samplePoint) {
;;     return length(samplePoint) - 1.0;
;; }

(defun-g rm-sphere ((p :vec3))
  (- (length p) 1.0))


;; /**
;;  * Signed distance function describing the scene.
;;  * 
;;  * Absolute value of the return value indicates the distance to the surface.
;;  * Sign indicates whether the point is inside or outside the surface,
;;  * negative indicating inside.
;;  */
;; float sceneSDF(vec3 samplePoint) {
;;     return sphereSDF(samplePoint);
;; }
(defun-g rm-scene ((sample-point :vec3))
  (rm-sphere sample-point))

;; /**
;;  * Return the shortest distance from the eyepoint to the scene surface along
;;  * the marching direction. If no part of the surface is found between start and end,
;;  * return end.
;;  * 
;;  * eye: the eye point, acting as the origin of the ray
;;  * marchingDirection: the normalized direction to march in
;;  * start: the starting distance away from the eye
;;  * end: the max distance away from the ey to march before giving up
;;  */
;; float shortestDistanceToSurface(vec3 eye, vec3 marchingDirection, float start, float end) {
;;     float depth = start;
;;     for (int i = 0; i < MAX_MARCHING_STEPS; i++) {
;;         float dist = sceneSDF(eye + depth * marchingDirection);
;;         if (dist < EPSILON) {
;; 			return depth;
;;         }
;;         depth += dist;
;;         if (depth >= end) {
;;             return end;
;;         }
;;     }
;;     return end;
;; }
(defun-g shortest-distance-to-surface ((eye :vec3)
                                       (marching-direction :vec3)
                                       (start :float)
                                       (end :float))
  (let* ((depth start))
    (for (i 0) (< i 255) (++ i)
         (let* ((dist (rm-scene (+ eye (* depth marching-direction)))))
           (when (< dist .0001)
               depth)))))

#|
/**
 * Return the normalized direction to march in from the eye point for a single pixel.
 * 
 * fieldOfView: vertical field of view in degrees
 * size: resolution of the output image
 * fragCoord: the x,y coordinate of the pixel in the output image
 */
vec3 rayDirection(float fieldOfView, vec2 size, vec2 fragCoord) {
    vec2 xy = fragCoord - size / 2.0;
    float z = size.y / tan(radians(fieldOfView) / 2.0);
    return normalize(vec3(xy, -z));
}


void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
	vec3 dir = rayDirection(45.0, iResolution.xy, fragCoord);
    vec3 eye = vec3(0.0, 0.0, 5.0);
    float dist = shortestDistanceToSurface(eye, dir, MIN_DIST, MAX_DIST);
    
    if (dist > MAX_DIST - EPSILON) {
        // Didn't hit anything
        fragColor = vec4(0.0, 0.0, 0.0, 0.0);
		return;
    }
    
    fragColor = vec4(1.0, 0.0, 0.0, 1.0);
}
|#

(defun-g vert ((vert :vec2))
  (v! vert 0 1))

(defun-g frag (&uniform
               (resolution :vec2)
               (time :float))
  (let* ((max-marching-steps 255)
         (min-dist 0f0)
         (max-dist 100f0)
         (epsilon 0.0001))
    (v! 1 1 0 0)))

(defun-g frag (&uniform
               (resolution :vec2)
               (time :float))
  (let* ((c (s~ gl-frag-coord :xy))
         (cc (/ time 100))
         (c (* c (mod cc 10)))
         (r (/ c resolution)))
    (v3! (* (x r) (y r)))))

(defpipeline-g mario ()
  :vertex   (vert :vec2)
  :fragment (frag))

(defun reset ())

(defun mynow ()
  (* .1 (get-internal-real-time)))

(defun game-step ()
  (clear)
  (map-g #'mario (get-quad-stream-v2)
         :time (mynow)
         :resolution (viewport-resolution
                      (current-viewport)))
  (swap))

(def-simple-main-loop runplay (:on-start #'reset)
  (game-step))

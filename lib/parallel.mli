val map : f:('a -> 'b) -> 'a list -> 'b list
(** [map ~f lst] Applies f to each element of lst and returns the resulting list.
    This is done concurrently using threads, so f needs to be thread-safe. *)

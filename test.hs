transpose :: [[a]]-> [[a]]
transpose ([]:_) = []
transpose x      = (map head x) : transpose (map tail x)
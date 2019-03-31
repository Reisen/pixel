export interface image {
    createdAt:  string;
    hash:       string;
    resolution: string;
    tags:       string[];
    uploader:   string;
    path:       string;
}

export interface tags {
    [index: string]: number
}

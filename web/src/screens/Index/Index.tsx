import React         from 'react';
import NavigationBar from '../../components/NavigationBar';
import SearchSidebar from './components/SearchSideBar';
import ImageGrid     from './components/ImageGrid';
import styles        from './Index.module.css';

interface Image {
    hash: string;
    tags: string[];
    uploader: string;
    createdAt: string;
}

const tags: string[] = [
    "dog",
    "field",
    "happy_dog",
    "bernese"
];

const imgs: Image[] = [
    { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999" },
    { "hash": "2398fh23", tags: [], uploader: "Reisen", createdAt: "01-02-1999" },
    { "hash": "848djslk", tags: [], uploader: "Reisen", createdAt: "01-02-1999" },
    { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999" },
    { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999" },
    { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999" },
    { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999" },
    { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999" },
    { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999" },
    { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999" },
    { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999" },
    { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999" },
    { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999" },
    { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999" },
    { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999" },
    { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999" },
    { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999" },
    { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999" },
    { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999" },
    { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999" },
    { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999" },
];

const Index = () => (
    <div className="Page">
        <NavigationBar />

        <div className={styles.PanelContainer}>
            <SearchSidebar tags={tags} />
            <ImageGrid width={6} images={imgs} />
        </div>
    </div>
);

export default Index

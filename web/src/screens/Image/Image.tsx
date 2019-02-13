import React         from 'react';
import NavigationBar from '../../components/NavigationBar';
import SearchSidebar from '../../components/SearchSideBar';
import ImagePanel    from './components/ImagePanel';
import styles        from './Image.module.css';
import { image }     from '../../types/image';

const onlyImage: image = {
    createdAt: "01/01/1999",
    hash: "asd98u23",
    resolution: "800x600",
    tags: [],
    uploader: "Reisen"
};

interface Props {
    image?: image;
}

const Image = (props: Props) => (
    <div className="Page">
        <NavigationBar />

        <div className={styles.Root}>
            <SearchSidebar tags={onlyImage.tags} />
            <ImagePanel image={onlyImage} />
        </div>
    </div>
);

export default Image;

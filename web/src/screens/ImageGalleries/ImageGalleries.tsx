import React, { useEffect } from 'react';
import { State }            from '../../store';
import { connect }          from 'react-redux';
import { fetchImages }      from '../../store/images';
import { image }            from '../../types/image';

// Components
import ImageGalleries       from './components/ImageGalleries';
import NavigationBar        from '../../components/NavigationBar';
import SearchSidebar        from '../../components/SearchSideBar';
import styles               from './ImageGalleries.module.css';


const onlyImage: image = {
    createdAt: '01/01/1999',
    hash: 'asd98u23',
    path: 'https://i.imgur.com/aG7C0Zx.jpg',
    resolution: '800x600',
    tags: [
        "cartoon",
        "example",
        "apple"
    ],
    uploader: 'Reisen',
};

interface Props {
    image?: image;
    username: string;
    fetchImages: () => void;
}

const Image = (props: Props) => {
    const { fetchImages } = props
    useEffect(() => { fetchImages() }, [fetchImages]);
    return (
        <div className="Page">
            <NavigationBar links={[]} username={props.username} />
            <div className={styles.Root}>
                <SearchSidebar initialPanel="metadata" />
                <ImageGalleries image={onlyImage} />
            </div>
        </div>
    );
}

const mapState = (state: State) => ({
    username: state.user.username
});

const mapDispatch = {
    fetchImages
};


export default connect(mapState, mapDispatch)(Image);

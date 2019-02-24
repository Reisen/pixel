import React, { useEffect } from 'react';
import styles               from './ImageView.module.css';
import { State }            from '../../store';
import { connect }          from 'react-redux';
import { fetchImages }      from '../../store/images';
import { image }            from '../../types/image';

import ImagePanel           from './components/ImagePanel';
import NavigationBar        from '../../components/NavigationBar';
import SearchSidebar        from '../../components/SearchSideBar';

const onlyImage: image = {
    createdAt: '01/01/1999',
    hash: 'asd98u23',
    path: 'https://i.imgur.com/aG7C0Zx.jpg',
    resolution: '800x600',
    tags: [],
    uploader: 'Reisen',
};

interface Props {
    image?: image;
    username: string;
    fetchImages: () => void;
}

const Image = (props: Props) => {
    useEffect(props.fetchImages);

    return (
        <div className="Page">
            <NavigationBar username={props.username} />

            <div className={styles.Root}>
                <SearchSidebar tags={onlyImage.tags} />
                <ImagePanel image={onlyImage} />
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

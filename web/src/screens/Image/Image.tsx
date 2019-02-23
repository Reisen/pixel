import React         from 'react';
import NavigationBar from '../../components/NavigationBar';
import SearchSidebar from '../../components/SearchSideBar';
import ImagePanel    from './components/ImagePanel';
import styles        from './Image.module.css';
import { image }     from '../../types/image';
import { State }     from '../../store';
import { connect }   from 'react-redux';

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
}

const Image = (props: Props) => (
    <div className="Page">
        <NavigationBar username={props.username} />

        <div className={styles.Root}>
            <SearchSidebar tags={onlyImage.tags} />
            <ImagePanel image={onlyImage} />
        </div>
    </div>
);


export default connect(
    (state: State) => ({
        username: state.user.username
    })
)(Image);

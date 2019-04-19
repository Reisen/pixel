import React     from 'react';
import { Link }  from 'react-router-dom';

import Attribute from '../../components/Attribute';
import Button    from '../../components/Button';
import styles    from './MetaDataPanel.module.css';


const MetaDataPanel = () =>
    <div>
        <h1>Metadata</h1>

        <div className={styles.Attributes}>
            <Attribute name="Faces:" value="3" icon="panda-face" />
            <Attribute name="Location:" value="France" icon="location-pin" />
            <Attribute name="Resolution:" value="1920x1080" icon="picture" />
            <Attribute name="Size:" value="1.3MB" icon="ui-folder" />
            <Attribute name="Time:" value="13:78" icon="sand-clock" />
            <Attribute name="Uploaded:" value="23 March, 2019" icon="ui-calendar" />
            <Attribute name="Hash:" value="10c435b41257e5ae2ef4ae1c360b3d02a14d5175ab37" icon="unlock" />
        </div>

        <Button icon="arrow-left">Back</Button>
        <Link to={window.location.pathname + "/galleries"}>
            <Button icon="save">Manage Galleries</Button>
        </Link>
        <Button icon="heart">Favorite</Button>
        <Button icon="trash" danger>Delete</Button>
    </div>;

export default MetaDataPanel;
